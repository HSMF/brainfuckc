use compile::compile;

use crate::ast::Ctx;

mod ast;
pub mod llvm;
mod parser;

mod compile;

fn main() {
    let helloworld: ast::Action = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.".parse().unwrap();
    let ast::Action::Block(helloworld) = helloworld else {
        panic!();
    };
    // println!("{helloworld:#?}");
    Ctx::new(std::io::stdin(), std::io::stdout()).simulate(&helloworld);

    let llvm = compile(&helloworld);
    println!("{llvm}");
}
