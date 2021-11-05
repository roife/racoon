use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use itertools::Itertools;
use slotmap::SlotMap;

use crate::compiler::intrusive_linkedlist::IntrusiveLinkedList;
use crate::compiler::ir::arena::{FuncId, GlobalId};
use crate::compiler::ir::value::constant::Constant;
use crate::compiler::ir::value::inst::{BinaryInstOp, BranchInst, InstKind};
use crate::compiler::ir::value::ty::IrTy;
use crate::compiler::ir::value::value::{Operand, Value};

use super::{func::IrFunc, global::Global};

#[derive(Debug)]
pub struct Module {
    first_func: Option<FuncId>,
    pub first_global: Option<GlobalId>,

    global_arena: SlotMap<GlobalId, Global>,
    func_arena: SlotMap<FuncId, IrFunc>,
}

impl Module {
    pub fn new() -> Module {
        Module {
            first_func: None,
            first_global: None,
            global_arena: SlotMap::with_key(),
            func_arena: SlotMap::with_key(),
        }
    }

    pub fn build_func(&mut self, func: IrFunc) -> FuncId {
        let id = self.func_arena.insert(func);
        let func = self.func_arena.get_mut(id).unwrap();
        func.next = self.first_func;
        self.first_func = Some(id);
        id
    }

    pub fn build_global(&mut self, global: Global) -> GlobalId {
        let id = self.global_arena.insert(global);
        let global = self.global_arena.get_mut(id).unwrap();
        global.next = self.first_global;
        self.first_global = Some(id);
        id
    }

    pub fn get_func(&self, func_id: FuncId) -> Option<&IrFunc> {
        self.func_arena.get(func_id)
    }

    pub fn get_func_mut(&mut self, func_id: FuncId) -> Option<&mut IrFunc> {
        self.func_arena.get_mut(func_id)
    }

    pub fn get_global(&self, global_id: GlobalId) -> Option<&Global> {
        self.global_arena.get(global_id)
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (_, global) in self.global_arena.items_iter(self.first_global, None) {
            writeln!(f, "@{} = global {}", global.name, global.init_val)?;
        }

        for (_, func) in self.func_arena.items_iter(self.first_func, None) {
            let mut cnt = 0;
            let mut id_cnt_map = HashMap::new();
            let param_str = func.params.iter()
                .map(|&param_id| {
                    let param = func.get_param(param_id).unwrap();
                    let s = format!("{} %{}", param.ty, cnt);
                    id_cnt_map.insert(Operand::Param(param_id), cnt);
                    cnt += 1;
                    s
                })
                .join(", ");
            writeln!(f, "define {} @{}({}) {{", func.ret_ty, func.name, param_str)?;

            let print_operand = |map: &HashMap<Operand, i32>, operand: &Operand| -> String {
                match operand {
                    Operand::Inst(x) => {
                        let ty = &func.inst_arena.get(*x).unwrap().ty;
                        let val = map.get(operand).unwrap();
                        format!("{} %{}", ty, val)
                    }
                    Operand::Constant(x) => {
                        let ty = x.get_ty();
                        match x {
                            Constant::Int(x) => format!("{} {}", ty, x),
                            _ => unreachable!()
                        }
                    }
                    Operand::Global(x) => {
                        let ty = &self.global_arena.get(*x).unwrap().ty;
                        let val = &self.global_arena.get(*x).unwrap().name;
                        format!("{} @{}", ty, val)
                    }
                    Operand::Param(x) => {
                        let ty = &func.get_param(*x).unwrap().ty;
                        let val = map.get(operand).unwrap();
                        format!("{} {}", ty, val)
                    }
                    Operand::BB(x) => {
                        // format!("{} {}", IrTy::Label, map.get(&Operand::BB(*x)).unwrap())
                        format!("{} {}", IrTy::Label, x)
                    },
                }
            };
            for (bb_id, bb) in func.bb_arena.items_iter(func.first_block, None) {
                writeln!(f, "{}:", cnt)?;
                id_cnt_map.insert(Operand::BB(bb_id), cnt);
                cnt += 1;

                let mut inst_ptr = bb.insts_head;
                while let Some(inst_id) = inst_ptr {
                    let inst = func.get_inst(inst_id).unwrap();
                    // dbg!(inst.clone());
                    match &inst.kind {
                        InstKind::Binary(binary_inst) => {
                            let op = match binary_inst.op {
                                BinaryInstOp::Add => "add",
                                BinaryInstOp::Sub => "sub",
                                BinaryInstOp::Mul => "mul",
                                BinaryInstOp::Div => "sdiv",
                                BinaryInstOp::Mod => "srem",
                                BinaryInstOp::Lt => "icmp slt",
                                BinaryInstOp::Le => "icmp sle",
                                BinaryInstOp::Gt => "icmp sgt",
                                BinaryInstOp::Ge => "icmp sge",
                                BinaryInstOp::Eq => "icmp eq",
                                BinaryInstOp::Ne => "icmp ne",
                                BinaryInstOp::And => todo!(),
                                BinaryInstOp::Or => todo!(),
                            };
                            writeln!(f, "%{} = {} {}, {}", cnt, op, print_operand(&id_cnt_map, &binary_inst.left), print_operand(&id_cnt_map, &binary_inst.right))?;
                            id_cnt_map.insert(Operand::Inst(inst_id), cnt);
                            cnt += 1;
                        }
                        InstKind::Branch(branch_inst) => {
                            match branch_inst {
                                BranchInst::Br { cond, true_bb, false_bb } => {
                                    writeln!(f, "br {}, {}, {}", print_operand(&id_cnt_map, cond), print_operand(&id_cnt_map, &Operand::BB(*true_bb)), print_operand(&id_cnt_map, &Operand::BB(*false_bb)))?
                                }
                                BranchInst::Jump { nxt_bb } => {
                                    writeln!(f, "br {}", print_operand(&id_cnt_map, &Operand::BB(*nxt_bb)))?
                                }
                            };
                        }
                        InstKind::ReturnInst(return_inst) => {
                            match &return_inst.val {
                                None => writeln!(f, "ret void")?,
                                Some(operand) => writeln!(f, "ret {}", print_operand(&id_cnt_map, operand))?,
                            };
                        }
                        InstKind::Alloca(alloca_inst) => {
                            writeln!(f, "%{} = alloca {}", cnt, alloca_inst.alloca_ty)?;
                            id_cnt_map.insert(Operand::Inst(inst_id), cnt);
                            cnt += 1;
                        }
                        InstKind::Load(load_inst) => {
                            writeln!(f, "%{} = load {}, {}", cnt, inst.ty, print_operand(&id_cnt_map, &load_inst.addr))?;
                            id_cnt_map.insert(Operand::Inst(inst_id), cnt);
                            cnt += 1;
                        }
                        InstKind::Store(store_inst) => {
                            writeln!(f, "store {}, {}", print_operand(&id_cnt_map, &store_inst.data), print_operand(&id_cnt_map, &store_inst.addr))?;
                        }
                        InstKind::GEP(gep_inst) => {
                            let indices_str = gep_inst.indices.iter().map(|x| print_operand(&id_cnt_map, x)).join(", ");
                            writeln!(f, "%{} = getelementptr {}, {}, {}", cnt, inst.ty, print_operand(&id_cnt_map, &gep_inst.ptr), indices_str)?
                        }
                        InstKind::ZExt(zext_inst) => {
                            writeln!(f, "%{} = zext {} to {}", cnt, print_operand(&id_cnt_map, &zext_inst.ori_val), zext_inst.target_ty)?
                        }
                        InstKind::Call(call_inst) => {
                            let callee = self.func_arena.get(call_inst.func).unwrap();
                            match inst.ty {
                                IrTy::Void => write!(f, "call void")?,
                                IrTy::Int(_) => {
                                    write!(f, "%{} = call", cnt)?;
                                    id_cnt_map.insert(Operand::Inst(inst_id), cnt);
                                    cnt += 1;
                                },
                                _ => unreachable!()
                            }
                            let args_str = call_inst.args.iter().map(|x| print_operand(&id_cnt_map, x)).join(", ");
                            writeln!(f, "@{}({})", callee.name, args_str)?
                        }
                    }
                    inst_ptr = inst.next;
                }
            }
            writeln!(f, "}}")?;
        }
        Ok(())
    }
}