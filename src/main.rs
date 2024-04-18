use crate::ast::Ctx;

mod ast;
pub mod llvm;
mod parser;

fn main() {
    let helloworld: ast::Action = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.".parse().unwrap();
    let ast::Action::Block(helloworld) = helloworld else {
        panic!();
    };
    // println!("{helloworld:#?}");
    Ctx::new(std::io::stdin(), std::io::stdout()).simulate(&helloworld);
}
