use std::io::{Read, Write};

#[no_mangle]
pub extern "C" fn print_char(ch: u8) {
    std::io::stdout().write_all(&[ch]).expect("failed to write");
}

#[no_mangle]
pub extern "C" fn get_char() -> u8 {
    let mut buf = [0];
    let i = std::io::stdin().read(&mut buf).expect("failed to read");
    if i == 1 {
        buf[0]
    } else {
        b'?'
    }
}

#[no_mangle]
pub extern "C" fn flush() {
    std::io::stdout().flush().unwrap()
}

#[no_mangle]
pub extern "C" fn init() {}
