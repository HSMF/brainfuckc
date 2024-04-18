use itertools::Itertools;
use std::fmt::Display;

fn seq<I, T>(f: &mut std::fmt::Formatter, it: I, sep: &str) -> std::fmt::Result
where
    I: Iterator<Item = T>,
    T: Display,
{
    for (i, el) in it.enumerate() {
        if i != 0 {
            write!(f, "{}", sep)?;
        }

        write!(f, "{el}")?;
    }

    Ok(())
}

struct Seq<'a, I>(&'static str, &'a [I]);

impl<'a, I> Display for Seq<'a, I>
where
    I: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        seq(f, self.1.iter(), self.0)
    }
}

/// local identifier
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Uid(String);

/// global identifier
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Gid(String);

/// named type
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Tid(String);

/// label
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Lbl(String);

impl Display for Uid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl Display for Gid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}

impl Display for Tid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for Lbl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Type {
    Void,
    Bool,
    Int,
    Byte,
    Fun(Vec<Type>, Box<Type>),
    Ptr(Box<Type>),
    Struct(Vec<Type>),
    Array(usize, Box<Type>),
    Named(Tid),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Void => write!(f, "void"),
            Type::Bool => write!(f, "i1"),
            Type::Int => write!(f, "i64"),
            Type::Byte => write!(f, "i8"),
            Type::Fun(args, ret) => write!(f, "{} ({})", ret, Seq(", ", args)),
            Type::Ptr(_) => write!(f, "ptr"),
            Type::Struct(ts) => write!(f, "{}", Seq(", ", ts)),
            Type::Array(n, t) => write!(f, "[{n} x {t}]"),
            Type::Named(t) => write!(f, "{t}"),
        }
    }
}

pub enum Operand {
    Null,
    Id(Uid),
    Gid(Gid),
    ConstInt(i64),
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Null => write!(f, "null"),
            Operand::Id(i) => write!(f, "{i}"),
            Operand::Gid(g) => write!(f, "{g}"),
            Operand::ConstInt(i) => write!(f, "{i}"),
        }
    }
}

pub enum Bop {
    Add,
    Sub,
    Mul,
    UDiv,
    SDiv,
    URem,
    SRem,
    Shl,
    LShr,
    AShr,
    And,
    Or,
    Xor,
}

impl Display for Bop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Bop::Add => write!(f, "add"),
            Bop::Sub => write!(f, "sub"),
            Bop::Mul => write!(f, "mul"),
            Bop::UDiv => write!(f, "udiv"),
            Bop::SDiv => write!(f, "sdiv"),
            Bop::URem => write!(f, "urem"),
            Bop::SRem => write!(f, "srem"),
            Bop::Shl => write!(f, "shl"),
            Bop::LShr => write!(f, "lshr"),
            Bop::AShr => write!(f, "ashr"),
            Bop::And => write!(f, "and"),
            Bop::Or => write!(f, "or"),
            Bop::Xor => write!(f, "xor"),
        }
    }
}

/// type of comparison operator
pub enum Cnd {
    Eq,
    Ne,
    UGt,
    UGe,
    ULt,
    Ule,
    SGt,
    SGe,
    SLt,
    SLe,
}

impl Display for Cnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Cnd::Eq => write!(f, "eq"),
            Cnd::Ne => write!(f, "ne"),
            Cnd::UGt => write!(f, "ugt"),
            Cnd::UGe => write!(f, "uge"),
            Cnd::ULt => write!(f, "ult"),
            Cnd::Ule => write!(f, "ule"),
            Cnd::SGt => write!(f, "sgt"),
            Cnd::SGe => write!(f, "sge"),
            Cnd::SLt => write!(f, "slt"),
            Cnd::SLe => write!(f, "sle"),
        }
    }
}

/// instruction
pub enum Insn {
    Binop(Bop, Type, Operand, Operand),
    ICmp(Cnd, Type, Operand, Operand),
    Alloca(Type),
    Load(Type, Operand),
    Store(Type, Operand, Operand),
    Call(Type, Operand, Vec<(Type, Operand)>),
    Gep(Type, Operand, Vec<Operand>),
    Bitcast(Type, Operand, Type),
    Comment(String),
}

impl Display for Insn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Insn::Binop(which, ty, op1, op2) => write!(f, "{which} {ty} {op1}, {op2}"),
            Insn::ICmp(cnd, ty, op1, op2) => write!(f, "icmp {cnd} {ty} {op1}, {op2}"),
            Insn::Alloca(ty) => write!(f, "alloca {ty}"),
            Insn::Load(ty, ptr) => write!(f, "load {ty}, ptr {ptr}"),
            Insn::Store(ty, src, dest) => write!(f, "store {ty} {src}, ptr {dest}"),
            Insn::Call(ty, fun, args) => {
                let args = args
                    .iter()
                    .format_with(", ", |(ty, op), f| f(&format_args!("{ty} {op}")));
                write!(f, "call {ty} {fun}({})", args)
            }
            Insn::Gep(ty, a, idx) if idx.is_empty() => {
                write!(f, "getelementptr {ty}, ptr {a}")
            }
            Insn::Gep(ty, a, indices) => {
                write!(
                    f,
                    "getelementptr {ty}, ptr {a}, {}",
                    indices
                        .iter()
                        .format_with(", ", |elt, f| f(&format_args!("i64 {elt}")))
                )
            }
            Insn::Bitcast(t1, op, t2) => write!(f, "bitcast {t1} {op} to {t2}"),
            Insn::Comment(s) => write!(f, "; {s}"),
        }
    }
}

pub struct Instruction(Uid, Insn);

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.1 {
            Insn::Store(_, _, _) | Insn::Comment(_) | Insn::Call(Type::Void, _, _) => {
                write!(f, "{}", self.1)
            }
            _ => write!(f, "{} = {}", self.0, self.1),
        }
    }
}

/// terminator instruction
pub enum Term {
    RetVoid,
    Ret(Type, Operand),
    Br(Operand, Lbl, Lbl),
    BrUncond(Lbl),
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

/// block as defined by llvm
pub struct Block {
    insns: Vec<Instruction>,
    term: (Uid, Term),
}

/// control flow graph
pub struct Cfg {
    entry: Block,
    labeled: Vec<(Lbl, Block)>,
}

/// function declaration
pub struct Fdecl {
    arg_types: Vec<Type>,
    return_type: Type,
    param: Vec<Uid>,
    body: Cfg,
}

/// global initializer
pub enum Ginit {
    Null,
    Gid(Gid),
    ConstInt(i64),
    String(String),
    Array(Vec<(Type, Ginit)>),
    Struct(Vec<(Type, Ginit)>),
    Bitcast(Type, Box<Ginit>, Type),
}

/// global declaration
struct Gdecl(Type, Ginit);

/// program
pub struct Program {
    type_decls: Vec<(Tid, Type)>,
    func_decls: Vec<(Gid, Fdecl)>,
    global_decls: Vec<(Gid, Gdecl)>,
    extern_decls: Vec<(Gid, Type)>,
}
