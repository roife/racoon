use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use itertools::Itertools;
use slotmap::SlotMap;

use crate::compiler::intrusive_linkedlist::IntrusiveLinkedList;
use crate::compiler::ir::arena::{FuncId, GlobalId};
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
            let get_operand_ty = |operand: &Operand| {
                match operand {
                    Operand::Inst(x) => func.inst_arena.get(*x).and_then(|x| Some(x.ty.clone())),
                    Operand::Constant(x) => Some(x.get_ty()),
                    Operand::Global(x) => self.global_arena.get(*x).and_then(|x| Some(x.ty.clone())),
                    Operand::Param(x) => func.get_param(*x).and_then(|x| Some(x.ty.clone())),
                    Operand::BB(_) => Some(IrTy::Label),
                }
            };
            for (bb_id, bb) in func.bb_arena.items_iter(func.first_block, None) {
                writeln!(f, "{}:", cnt)?;
                id_cnt_map.insert(Operand::BB(bb_id), cnt);
                cnt += 1;

                let mut inst_ptr = bb.insts_head;
                while let Some(inst_id) = inst_ptr {
                    let inst = func.get_inst(inst_id).unwrap();
                    match &inst.kind {
                        InstKind::Binary(binary_inst) => {
                            let left_ty = get_operand_ty(&binary_inst.left).unwrap();
                            let left_id = id_cnt_map.get(&binary_inst.left).unwrap();
                            let right_ty = get_operand_ty(&binary_inst.right).unwrap();
                            let right_id = id_cnt_map.get(&binary_inst.right).unwrap();
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
                            writeln!(f, "%{} = {} {} {}, {} {}", cnt, op, left_ty, left_id, right_ty, right_id)?;
                            id_cnt_map.insert(Operand::Inst(inst_id), cnt);
                            cnt += 1;
                        }
                        InstKind::Branch(branch_inst) => {
                            // match branch_inst {
                            //     BranchInst::Br { cond, true_bb, false_bb } => {
                            //         let cond_id = i
                            //         writeln!(f, "br {}, {}, {}")
                            //     }
                            //     BranchInst::Jump { .. } => {}
                            // }
                        }
                        InstKind::ReturnInst(_) => {}
                        InstKind::Alloca(_) => {}
                        InstKind::Load(_) => {}
                        InstKind::Store(_) => {}
                        InstKind::GEP(_) => {}
                        InstKind::ZExt(_) => {}
                        InstKind::Call(_) => {}
                    }
                    inst_ptr = inst.next;
                }
            }
            writeln!(f, "}}")?;
        }
        Ok(())
    }
}