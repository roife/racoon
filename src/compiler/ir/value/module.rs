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
                        let ty = IrTy::ptr_of(self.global_arena.get(*x).unwrap().ty.clone());
                        let val = &self.global_arena.get(*x).unwrap().name;
                        format!("{} @{}", ty, val)
                    }
                    Operand::Param(x) => {
                        let ty = &func.get_param(*x).unwrap().ty;
                        let val = map.get(operand).unwrap();
                        format!("{} %{}", ty, val)
                    }
                    Operand::BB(x) => {
                        // format!("{} {}", IrTy::Label, map.get(&Operand::BB(*x)).unwrap())
                        format!("{} %{}", IrTy::Label, map.get(operand).unwrap())
                    },
                }
            };
            for (bb_id, bb) in func.bb_arena.items_iter(func.first_block, None) {
                id_cnt_map.insert(Operand::BB(bb_id), cnt);
                cnt += 1;

                let mut inst_ptr = bb.insts_head;
                while let Some(inst_id) = inst_ptr {
                    let inst = func.get_inst(inst_id).unwrap();
                    match &inst.kind {
                        InstKind::Binary(_) | InstKind::Alloca(_) | InstKind::Load(_) | InstKind::GEP(_) | InstKind::ZExt(_) => {
                            id_cnt_map.insert(Operand::Inst(inst_id), cnt);
                            cnt += 1;
                        }
                        InstKind::Call(_) => {
                            match inst.ty {
                                IrTy::Void => {},
                                IrTy::Int(_) => {
                                    id_cnt_map.insert(Operand::Inst(inst_id), cnt);
                                    cnt += 1;
                                },
                                _ => unreachable!()
                            }
                        }
                        _ => {}
                    }
                    inst_ptr = inst.next;
                }
            }

            for (bb_id, bb) in func.bb_arena.items_iter(func.first_block, None) {
                writeln!(f, "{}:", id_cnt_map.get(&Operand::BB(bb_id)).unwrap())?;

                let mut inst_ptr = bb.insts_head;
                while let Some(inst_id) = inst_ptr {
                    let inst = func.get_inst(inst_id).unwrap();
                    write!(f, "\t");
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
                            let rhs_str = match &binary_inst.right {
                                Operand::Inst(x) => {
                                    let val = id_cnt_map.get(&binary_inst.right).unwrap();
                                    format!("%{}", val)
                                }
                                Operand::Constant(x) => {
                                    match x {
                                        Constant::Int(x) => format!("{}", x),
                                        _ => unreachable!()
                                    }
                                }
                                Operand::Global(x) => {
                                    let val = &self.global_arena.get(*x).unwrap().name;
                                    format!("@{}", val)
                                }
                                Operand::Param(x) => {
                                    let val = id_cnt_map.get(&binary_inst.right).unwrap();
                                    format!("%{}", val)
                                }
                                Operand::BB(x) => {
                                    // format!("{} {}", IrTy::Label, map.get(&Operand::BB(*x)).unwrap())
                                    format!("%{}", id_cnt_map.get(&binary_inst.right).unwrap())
                                },
                            };
                            writeln!(f, "%{} = {} {}, {}", id_cnt_map.get(&Operand::Inst(inst_id)).unwrap(), op, print_operand(&id_cnt_map, &binary_inst.left), rhs_str)?;
                            // if let Operand::Constant(const_right) = &binary_inst.right {
                            //     match const_right {
                            //         Constant::Int(x) => writeln!(f, "%{} = {} {}, {}", id_cnt_map.get(&Operand::Inst(inst_id)).unwrap(), op, print_operand(&id_cnt_map, &binary_inst.left), x)?,
                            //         _ => unreachable!()
                            //     }
                            // } else {
                            //     writeln!(f, "%{} = {} {}, {}", id_cnt_map.get(&Operand::Inst(inst_id)).unwrap(), op, print_operand(&id_cnt_map, &binary_inst.left), print_operand(&id_cnt_map, &binary_inst.right))?;
                            // }
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
                            writeln!(f, "%{} = alloca {}", id_cnt_map.get(&Operand::Inst(inst_id)).unwrap(), alloca_inst.alloca_ty)?;
                        }
                        InstKind::Load(load_inst) => {
                            writeln!(f, "%{} = load {}, {}", id_cnt_map.get(&Operand::Inst(inst_id)).unwrap(), inst.ty, print_operand(&id_cnt_map, &load_inst.addr))?;
                        }
                        InstKind::Store(store_inst) => {
                            writeln!(f, "store {}, {}", print_operand(&id_cnt_map, &store_inst.data), print_operand(&id_cnt_map, &store_inst.addr))?;
                        }
                        InstKind::GEP(gep_inst) => {
                            let indices_str = gep_inst.indices.iter().map(|x| print_operand(&id_cnt_map, x)).join(", ");
                            let ty = match &gep_inst.ptr {
                                Operand::Inst(inst) => func.get_inst(*inst).unwrap().ty.clone(),
                                Operand::Constant(c) => c.get_ty(),
                                Operand::Global(g) => self.global_arena.get(*g).unwrap().ty.clone(),
                                Operand::Param(p) => func.get_param(*p).unwrap().ty.clone(),
                                Operand::BB(_) => IrTy::Label,
                            };
                            // todo
                            writeln!(f, "%{} = getelementptr {}, {}, i32 0, {}", id_cnt_map.get(&Operand::Inst(inst_id)).unwrap(), ty.clone(), print_operand(&id_cnt_map, &gep_inst.ptr), indices_str)?
                        }
                        InstKind::ZExt(zext_inst) => {
                            writeln!(f, "%{} = zext {} to {}", id_cnt_map.get(&Operand::Inst(inst_id)).unwrap(), print_operand(&id_cnt_map, &zext_inst.ori_val), zext_inst.target_ty)?
                        }
                        InstKind::Call(call_inst) => {
                            let callee = self.func_arena.get(call_inst.func).unwrap();
                            match inst.ty {
                                IrTy::Void => write!(f, "call void ")?,
                                IrTy::Int(_) => {
                                    write!(f, "%{} = call i32 ", id_cnt_map.get(&Operand::Inst(inst_id)).unwrap())?;
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