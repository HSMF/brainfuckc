use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ctx<I, O> {
    band: [u8; 1024],
    ptr: usize,
    sink: O,
    source: I,
}

impl<I, O> Ctx<I, O>
where
    I: std::io::Read,
    O: std::io::Write,
{
    pub fn new(i: I, o: O) -> Self {
        Ctx {
            band: [0; 1024],
            ptr: 0,
            sink: o,
            source: i,
        }
    }

    pub fn input(&mut self) -> u8 {
        let mut buf = [0];
        let i = self.source.read(&mut buf).expect("failed to read");
        if i != 1 {
            b'0'
        } else {
            b'?'
        }
    }

    pub fn output(&mut self, x: u8) {
        write!(self.sink, "{}", x as char).unwrap();
    }

    pub fn simulate(&mut self, code: &[Action]) {
        let mut i = 0;
        while i < code.len() {
            match &code[i] {
                Action::Right => self.ptr = (self.ptr + 1) % 1024,
                Action::Left => self.ptr = self.ptr.checked_sub(1).unwrap_or(1023),
                Action::Incr => self.band[self.ptr] = self.band[self.ptr].overflowing_add(1).0,
                Action::Decr => self.band[self.ptr] = self.band[self.ptr].overflowing_sub(1).0,
                Action::Print => self.output(self.band[self.ptr]),
                Action::Input => self.band[self.ptr] = self.input(),
                Action::Block(body) => {
                    if self.band[self.ptr] != 0 {
                        self.simulate(body);
                        if self.band[self.ptr] != 0 {
                            i -= 1;
                        }
                    }
                }
            }
            i += 1;
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    Right,
    Left,
    Incr,
    Decr,
    Print,
    Input,
    Block(Vec<Action>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingBracket;

fn expect_bracket(s: &str) -> Result<&str, MissingBracket> {
    let (a, b) = s.split_at(1);
    if a != "]" {
        return Err(MissingBracket);
    }

    Ok(b)
}

fn get_block(mut s: &str) -> Result<(Vec<Action>, &str), MissingBracket> {
    let mut chars = s.char_indices();
    let mut actions = vec![];
    while let Some((i, ch)) = chars.next() {
        match ch {
            '+' => actions.push(Action::Incr),
            '-' => actions.push(Action::Decr),
            '>' => actions.push(Action::Right),
            '<' => actions.push(Action::Left),
            '.' => actions.push(Action::Print),
            ',' => actions.push(Action::Input),
            '[' => {
                let (acts, new_s) = get_block(&s[i + 1..])?;
                s = new_s;
                s = expect_bracket(s)?;
                chars = s.char_indices();
                actions.push(Action::Block(acts))
            }
            ']' => {
                s = &s[i..];
                break;
            }
            _ => {}
        }
    }
    Ok((actions, s))
}

impl FromStr for Action {
    type Err = MissingBracket;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (actions, _) = get_block(s)?;

        Ok(Self::Block(actions))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn expect_output(code: &str, expected: &str) {
        let actions: Action = code.parse().unwrap();
        let Action::Block(actions) = actions else {
            panic!();
        };

        let expected_buf = expected.as_bytes();
        let mut buf = Vec::with_capacity(expected_buf.len());
        Ctx::new([].as_slice(), &mut buf).simulate(&actions);

        assert_eq!(
            buf,
            expected_buf,
            "expected {} but got {}",
            expected,
            std::str::from_utf8(expected_buf).unwrap_or("<NOT UTF-8>")
        );
    }

    #[test]
    fn hello_world() {
        expect_output("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.", "Hello World!\n");
    }

    #[test]
    fn hello_world_hard() {
        expect_output(
            ">++++++++[-<+++++++++>]
             <.>>+>-[+]++>++>+++[>[-
             >+++<<+++>]<<]>-----.>-
             >+++..+++.>-.<<+[>[+>+]
             >>]<--------------.>>.+
             ++.------.--------.>+.>+.",
            "Hello World!\n",
        );
    }
}
