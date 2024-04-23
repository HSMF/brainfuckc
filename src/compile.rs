use std::fmt::Debug;

use crate::ast::Action;

use llvm::{
    self, Block, Bop, Cfg, Fdecl, Gdecl, Gid, Insn, Instruction, Label, Operand, Term, Type, Uid,
};
struct Ctx {
    counter: usize,
}

impl Ctx {
    fn new() -> Self {
        Ctx { counter: 0 }
    }

    fn next(&mut self) -> String {
        self.counter += 1;
        format!("i{}", self.counter)
    }

    fn next_lbl(&mut self) -> Label {
        self.counter += 1;
        Label::new(format!("l{}", self.counter))
    }
}

enum Elt {
    I(Instruction),
    T(Term),
    L(Label),
}

#[derive(Default)]
struct Stream(Vec<Elt>);

impl Debug for Stream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in &self.0 {
            match i {
                Elt::I(i) => writeln!(f, "I {i}")?,
                Elt::T(t) => writeln!(f, "T {t}")?,
                Elt::L(t) => writeln!(f, "L {t}")?,
            }
        }
        Ok(())
    }
}

impl Stream {
    pub fn push(&mut self, e: Elt) {
        self.0.push(e)
    }

    pub fn pushi(&mut self, e: Instruction) {
        self.0.push(Elt::I(e))
    }

    pub fn pusht(&mut self, e: Term) {
        self.0.push(Elt::T(e))
    }
}

impl From<Stream> for Cfg {
    fn from(value: Stream) -> Self {
        let mut cur_block = Vec::new();
        let mut cur_term = None;

        let mut labeled_blocks = Vec::new();

        for el in value.0.into_iter().rev() {
            match el {
                Elt::I(i) => {
                    cur_block.push(i);
                }
                Elt::T(t) => {
                    cur_block.clear();
                    cur_term = Some(t);
                }
                Elt::L(l) => {
                    let block = Block::new(
                        cur_block.drain(..).rev().collect(),
                        cur_term.take().expect("missing terminator"),
                    );
                    labeled_blocks.push((l, block));
                }
            }
        }

        Cfg::new(
            Block::new(cur_block.drain(..).rev().collect(), cur_term.unwrap()),
            labeled_blocks,
        )
    }
}

fn compile_action(ctx: &mut Ctx, action: &Action, mut stream: Stream) -> Stream {
    fn movement(bop: Bop, ctx: &mut Ctx, stream: &mut Stream) {
        let cur_pos = Uid::new(ctx.next());
        let new_pos = Uid::new(ctx.next());
        let in_range_pos = Uid::new(ctx.next());

        stream.pushi(Instruction(
            cur_pos.clone(),
            Insn::Load(Type::i64(), Operand::Id(Uid::new("cursor"))),
        ));
        stream.pushi(Instruction(
            new_pos.clone(),
            Insn::Binop(bop, Type::i64(), Operand::Id(cur_pos), Operand::ConstInt(1)),
        ));

        stream.pushi(Instruction(
            in_range_pos.clone(),
            Insn::Binop(
                Bop::SRem,
                Type::i64(),
                Operand::Id(new_pos),
                Operand::ConstInt(1024),
            ),
        ));

        stream.pushi(Instruction::unnamed(Insn::Store(
            Type::i64(),
            Operand::Id(in_range_pos),
            Operand::Id(Uid::new("cursor")),
        )))
    }

    fn change_cell(bop: Bop, ctx: &mut Ctx, stream: &mut Stream) {
        let cursor_val = Uid::new(ctx.next());
        let addr = Uid::new(ctx.next());
        let cur_val = Uid::new(ctx.next());
        let new_val = Uid::new(ctx.next());

        stream.pushi(Instruction(
            cursor_val.clone(),
            Insn::Load(Type::i64() , Operand::Id(Uid::new("cursor"))),
        ));
        stream.pushi(Instruction(
            addr.clone(),
            Insn::Gep(
                Type::i8(),
                Operand::Gid(Gid::new("band")),
                vec![Operand::Id(cursor_val)],
            ),
        ));
        stream.pushi(Instruction(
            cur_val.clone(),
            Insn::Load(Type::i8(), Operand::Id(addr.clone())),
        ));
        stream.pushi(Instruction(
            new_val.clone(),
            Insn::Binop(bop, Type::i8(), Operand::Id(cur_val), Operand::ConstInt(1)),
        ));

        stream.pushi(Instruction::unnamed(Insn::Store(
            Type::i8(),
            Operand::Id(new_val),
            Operand::Id(addr),
        )))
    }

    match action {
        Action::Right => {
            movement(Bop::Add, ctx, &mut stream);
        }
        Action::Left => {
            movement(Bop::Sub, ctx, &mut stream);
        }
        Action::Incr => {
            change_cell(Bop::Add, ctx, &mut stream);
        }
        Action::Decr => {
            change_cell(Bop::Sub, ctx, &mut stream);
        }
        Action::Print => {
            let cursor_val = Uid::new(ctx.next());
            let addr = Uid::new(ctx.next());
            let cur_val = Uid::new(ctx.next());

            stream.pushi(Instruction(
                cursor_val.clone(),
                Insn::Load(Type::i64(), Operand::Id(Uid::new("cursor"))),
            ));
            stream.pushi(Instruction(
                addr.clone(),
                Insn::Gep(
                    Type::i8(),
                    Operand::Gid(Gid::new("band")),
                    vec![Operand::Id(cursor_val)],
                ),
            ));
            stream.pushi(Instruction(
                cur_val.clone(),
                Insn::Load(Type::i8(), Operand::Id(addr.clone())),
            ));
            stream.pushi(Instruction::unnamed(Insn::Call(
                Type::Void,
                Operand::Gid(Gid::new("print_char")),
                vec![(Type::i8(), Operand::Id(cur_val))],
            )))
        }
        Action::Input => todo!(),
        Action::Block(acts) => {
            let entry = ctx.next_lbl();
            let after = ctx.next_lbl();
            let body = ctx.next_lbl();
            let cursor_val = Uid::new(ctx.next());
            let addr = Uid::new(ctx.next());
            let cur_val = Uid::new(ctx.next());

            let is_zero = Uid::new(ctx.next());

            stream.pusht(Term::BrUncond(entry.clone()));
            stream.push(Elt::L(entry.clone()));

            stream.pushi(Instruction(
                cursor_val.clone(),
                Insn::Load(Type::i64(), Operand::Id(Uid::new("cursor"))),
            ));
            stream.pushi(Instruction(
                addr.clone(),
                Insn::Gep(
                    Type::i8(),
                    Operand::Gid(Gid::new("band")),
                    vec![Operand::Id(cursor_val)],
                ),
            ));
            stream.pushi(Instruction(
                cur_val.clone(),
                Insn::Load(Type::i8(), Operand::Id(addr.clone())),
            ));

            stream.pushi(Instruction(
                is_zero.clone(),
                Insn::ICmp(
                    llvm::Cnd::Eq,
                    Type::i8(),
                    Operand::Id(cur_val),
                    Operand::ConstInt(0),
                ),
            ));

            stream.pusht(Term::Br(Operand::Id(is_zero), after.clone(), body.clone()));
            stream.push(Elt::L(body));

            stream = acts
                .iter()
                .fold(stream, |stream, action| compile_action(ctx, action, stream));

            stream.pusht(Term::BrUncond(entry));
            stream.push(Elt::L(after));
        }
    }
    stream
}

pub fn compile(code: &[Action]) -> llvm::Program {
    let mut program = llvm::Program::new().with_extern_decls(vec![
        (
            Gid::new("print_char"),
            Type::Fun(vec![Type::i8()], Box::new(Type::Void)),
        ),
        (
            Gid::new("get_char"),
            Type::Fun(vec![], Box::new(Type::i8())),
        ),
        (Gid::new("flush"), Type::Fun(vec![], Box::new(Type::Void))),
        (Gid::new("init"), Type::Fun(vec![], Box::new(Type::Void))),
    ]);
    program.add_global_decl(
        Gid::new("band"),
        Gdecl::new(
            Type::Array(1024, Box::new(Type::i8())),
            llvm::Ginit::Zeroinit,
        ),
    );

    let mut ctx = Ctx::new();

    let mut stream = Stream::default();
    stream.pushi(Instruction::unnamed(Insn::Call(
        Type::Void,
        Operand::Gid(Gid::new("init")),
        vec![],
    )));
    stream.pushi(Instruction(Uid::new("cursor"), Insn::Alloca(Type::i64())));
    stream.pushi(Instruction::unnamed(Insn::Store(
        Type::i64(),
        Operand::ConstInt(0),
        Operand::Id(Uid::new("cursor")),
    )));

    let mut stream = code.iter().fold(stream, |stream, action| {
        compile_action(&mut ctx, action, stream)
    });

    stream.pushi(Instruction::unnamed(Insn::Call(
        Type::Void,
        Operand::Gid(Gid::new("flush")),
        vec![],
    )));
    stream.pusht(Term::Ret(Type::i64(), Operand::ConstInt(0)));

    // println!("{stream:?}");
    let cfg: Cfg = stream.into();

    program.add_func_decl(Fdecl::new(Gid::new("main"), vec![], Type::i64(), cfg));
    program
}
