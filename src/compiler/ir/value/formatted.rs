use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use itertools::Itertools;

use crate::compiler::intrusive_linkedlist::IntrusiveLinkedList;
use crate::compiler::ir::value::constant::Constant;
use crate::compiler::ir::value::func::IrFunc;
use crate::compiler::ir::value::inst::{BinaryInstOp, BrInst, InstKind};
use crate::compiler::ir::value::module::Module;
use crate::compiler::ir::value::ty::IrTy;
use crate::compiler::ir::value::value::{Operand, Value};

#[derive(Debug, Clone)]
struct VRegManager<'a> {
    cnt: usize,
    map: HashMap<Operand, usize>,
    module: &'a Module,
    func: &'a IrFunc,
}

impl<'a> VRegManager<'a> {
    pub fn new(module: &'a Module, func: &'a IrFunc) -> VRegManager<'a> {
        VRegManager {
            cnt: 0,
            map: HashMap::default(),
            module,
            func,
        }
    }

    pub fn build_vreg(&mut self, operand: Operand) -> usize {
        let id = self.cnt;
        self.cnt += 1;
        self.map.insert(operand, id);
        id
    }

    pub fn get_vreg(&self, operand: &Operand) -> Option<usize> {
        self.map.get(operand).copied()
    }

    pub fn get_vreg_unwrap(&self, operand: &Operand) -> usize {
        self.get_vreg(operand).unwrap()
    }

    pub fn print(&self, operand: &Operand) -> String {
        match operand {
            Operand::Inst(x) => {
                let ty = &self.func.inst_arena.get(*x).unwrap().ty;
                format!("{} %{}", ty, self.get_vreg_unwrap(operand))
            }
            Operand::Const(x) => {
                let ty = x.get_ty();
                match x {
                    Constant::Int(x) => format!("{} {}", ty, x),
                    _ => unreachable!()
                }
            }
            Operand::Global(x) => {
                let global = self.module.global_arena.get(*x).unwrap();
                let ty = &global.ty;
                let val = &global.name;
                format!("{} @{}", ty, val)
            }
            Operand::Param(x) => {
                let ty = &self.func.get_param(*x).unwrap().ty;
                format!("{} %{}", ty, self.get_vreg_unwrap(operand))
            }
            Operand::BB(_) => {
                format!("{} %{}", IrTy::Label, self.get_vreg_unwrap(operand))
            }
        }
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // print globals
        for (_, global) in self.global_arena.items_iter(self.first_global, None) {
            writeln!(f, "@{} = global {}", global.name, global.init_val)?;
            writeln!(f)?;
        }

        for (_, func) in self.func_arena.items_iter(self.first_func, None) {
            if func.is_builtin {
                let param_str = func.params.iter()
                    .map(|&param_id| {
                        let param = func.get_param(param_id).unwrap();
                        format!("{}", param.ty)
                    })
                    .join(", ");
                writeln!(f, "declare {} @{}({}) #1", func.ret_ty, func.name, param_str)?;
                writeln!(f)?;
                continue;
            }
            let mut vregs = VRegManager::new(self, func);

            // print params
            let param_str = func.params.iter()
                .map(|&param_id| {
                    let param = func.get_param(param_id).unwrap();
                    format!("{} %{}", param.ty, vregs.build_vreg(param_id.into()))
                })
                .join(", ");

            writeln!(f, "define {} @{}({}) {{", func.ret_ty, func.name, param_str)?;

            // build vregs
            for (bb_id, bb) in func.bb_arena.items_iter(func.first_block, None) {
                vregs.build_vreg(bb_id.into());

                let mut inst_iter = bb.insts_head;
                while let Some(inst_id) = inst_iter {
                    use InstKind::*;
                    let inst = func.get_inst(inst_id).unwrap();
                    match &inst.kind {
                        Binary(_) | Alloca(_) | Load(_) | GEP(_) | ZExt(_) => {
                            vregs.build_vreg(inst_id.into());
                        }
                        Call(_) if matches!(inst.ty, IrTy::Int(_)) => {
                            vregs.build_vreg(inst_id.into());
                        }
                        _ => {}
                    };
                    inst_iter = inst.next;
                }
            }

            for (bb_id, bb) in func.bb_arena.items_iter(func.first_block, None) {
                writeln!(f, "{}:", vregs.get_vreg_unwrap(&bb_id.into()))?;

                let mut inst_iter = bb.insts_head;
                while let Some(inst_id) = inst_iter {
                    write!(f, "\t")?;
                    let inst = func.get_inst(inst_id).unwrap();

                    match &inst.kind {
                        InstKind::Binary(binary_inst) => {
                            let dst = vregs.get_vreg_unwrap(&inst_id.into());
                            let lhs = vregs.print(&binary_inst.left);
                            let rhs = match &binary_inst.right {
                                Operand::Inst(_) | Operand::Param(_) => format!("%{}", vregs.get_vreg_unwrap(&binary_inst.right)),
                                Operand::Const(Constant::Int(x)) => format!("{}", x),
                                _ => unreachable!()
                            };

                            writeln!(f, "%{} = {} {}, {}", dst, binary_inst.op, lhs, rhs)?;
                        }
                        InstKind::Br(branch_inst) => {
                            match branch_inst {
                                BrInst::Br { cond, true_bb, false_bb } => {
                                    let cond = vregs.print(cond);
                                    let true_bb = vregs.print(&Operand::from(*true_bb));
                                    let false_bb = vregs.print(&Operand::from(*false_bb));
                                    writeln!(f, "br {}, {}, {}", cond, true_bb, false_bb)?;
                                }
                                BrInst::Jump { nxt_bb } => {
                                    let nxt_bb = vregs.print(&Operand::from(*nxt_bb));
                                    writeln!(f, "br {}", nxt_bb)?;
                                }
                            };
                        }
                        InstKind::RetInst(return_inst) => {
                            match &return_inst.val {
                                None => writeln!(f, "ret void")?,
                                Some(operand) => {
                                    let ret_val = vregs.print(operand);
                                    writeln!(f, "ret {}", ret_val)?;
                                },
                            };
                        }
                        InstKind::Alloca(alloca_inst) => {
                            let dst_ptr = vregs.get_vreg_unwrap(&Operand::from(inst_id));
                            writeln!(f, "%{} = alloca {}", dst_ptr, alloca_inst.alloca_ty)?;
                        }
                        InstKind::Load(load_inst) => {
                            let dst = vregs.get_vreg_unwrap(&Operand::from(inst_id));
                            let addr = vregs.print(&load_inst.addr);
                            writeln!(f, "%{} = load {}, {}", dst, inst.ty, addr)?;
                        }
                        InstKind::Store(store_inst) => {
                            let data = vregs.print(&store_inst.data);
                            let addr = vregs.print(&store_inst.addr);
                            writeln!(f, "store {}, {}", data, addr)?;
                        }
                        InstKind::GEP(gep_inst) => {
                            let indices = gep_inst.indices.iter()
                                .map(|x| vregs.print(x)).join(", ");
                            let ty = match &gep_inst.ptr {
                                Operand::Inst(inst) => &func.get_inst(*inst).unwrap().ty,
                                Operand::Global(g) => &self.global_arena.get(*g).unwrap().ty,
                                Operand::Param(p) => &func.get_param(*p).unwrap().ty,
                                _ => unreachable!()
                            };
                            let ty = IrTy::deptr_of(ty).unwrap();

                            let dst = vregs.get_vreg_unwrap(&Operand::from(inst_id));
                            let addr = vregs.print(&gep_inst.ptr);

                            writeln!(f, "%{} = getelementptr {}, {}, {}", dst, ty, addr, indices)?;
                        }
                        InstKind::ZExt(zext_inst) => {
                            let dst = vregs.get_vreg_unwrap(&Operand::from(inst_id));
                            let src = vregs.print(&zext_inst.ori_val);
                            writeln!(f, "%{} = zext {} to {}", dst, src, zext_inst.target_ty)?;
                        }
                        InstKind::Call(call_inst) => {
                            let callee = self.func_arena.get(call_inst.func_id).unwrap();
                            match inst.ty {
                                IrTy::Void => write!(f, "call void ")?,
                                IrTy::Int(_) => {
                                    let dst = vregs.get_vreg_unwrap(&Operand::from(inst_id));
                                    write!(f, "%{} = call i32 ", dst)?;
                                }
                                _ => unreachable!()
                            }
                            let args_str = call_inst.args.iter()
                                .map(|x| vregs.print(x))
                                .join(", ");
                            writeln!(f, "@{}({})", callee.name, args_str)?;
                        }
                    }
                    inst_iter = inst.next;
                }
            }
            writeln!(f, "}}")?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Display for BinaryInstOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use BinaryInstOp::*;
        let op_str = match &self {
            Add => "add",
            Sub => "sub",
            Mul => "mul",
            Div => "sdiv",
            Mod => "srem",
            Lt => "icmp slt",
            Le => "icmp sle",
            Gt => "icmp sgt",
            Ge => "icmp sge",
            Eq => "icmp eq",
            Ne => "icmp ne",
            And => "and",
            Or => "or",
        };
        write!(f, "{}", op_str)
    }
}