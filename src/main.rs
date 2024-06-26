use std::{fs::File, io::Write, process::Command};

use brainfuckc::compile::compile;

use brainfuckc::ast::Action;

fn main() -> anyhow::Result<()> {
    let s = if let Some(x) = std::env::args().nth(1) {
        x
    } else {
        let mut s = String::new();
        for line in std::io::stdin().lines() {
            let line = line.unwrap();
            s += &line;
            s += "\n";
        }
        s
    };
    // let s = std::env::args().nth(1).unwrap_or("
    //     ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]
    //     >>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
    //     ".to_string());
    let helloworld: Action = s.parse().unwrap();
    let Action::Block(helloworld) = helloworld else {
        panic!();
    };
    // println!("{helloworld:#?}");
    // Ctx::new(std::io::stdin(), std::io::stdout()).simulate(&helloworld);

    let llvm = compile(&helloworld);
    // println!("{llvm}");

    let mut f = File::create("target/tmp.ll")?;
    write!(f, "{llvm}")?;

    Command::new("clang")
        .args([
            "target/tmp.ll",
            "-c",
            "-Wno-override-module",
            "-O3",
            "-o",
            "target/tmp.o",
        ])
        .spawn()?
        .wait()?;
    Command::new("cargo")
        .args(["build", "--package", "stdlib"])
        .spawn()?
        .wait()?;
    Command::new("clang")
        .args([
            "target/tmp.o",
            "target/debug/libstdlib.dylib",
            "-O3",
            "-o",
            "target/tmp",
        ])
        .spawn()?
        .wait()?;
    Command::new("target/tmp").spawn()?.wait()?;
    Ok(())
}
