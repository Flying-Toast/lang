#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TokenKind<'a> {
    Let,
    Ident(&'a str),
    Equals,
    Plus,
    NumLit(&'a str),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub line: usize,
    pub col: usize,
}

pub struct TokenStream<'a> {
    src: &'a [u8],
    line: usize,
    col: usize,
}

fn tostr(x: &[u8]) -> &str {
    std::str::from_utf8(x).unwrap()
}

impl<'a> TokenStream<'a> {
    fn is_empty(&self) -> bool {
        self.src.is_empty()
    }

    fn next_char(&mut self) -> Option<u8> {
        if self.src.is_empty() {
            None
        } else {
            let ch = self.src[0];
            if ch == b'\n' {
                self.col = 1;
                self.line += 1;
            } else {
                self.col += 1;
            }
            self.src = &self.src[1..];
            Some(ch)
        }
    }

    fn peek_char(&self) -> Option<u8> {
        self.src.first().copied()
    }

    fn eat_str(&mut self, s: &[u8]) -> bool {
        if self.src.starts_with(s) {
            for _ in 0..s.len() {
                let _ = self.next_char();
            }
            true
        } else {
            false
        }
    }

    fn eat_while<F: Fn(u8) -> bool>(&mut self, f: F) -> &'a [u8] {
        let start = self.src;
        while let Some(next) = self.peek_char() {
            if f(next) {
                self.next_char();
            } else {
                break;
            }
        }
        let len_diff = start.len() - self.src.len();

        &start[..len_diff]
    }

    fn consume_whitespace(&mut self) {
        self.eat_while(|x| x == b' ' || x == b'\t' || x == b'\n');
    }

    fn make_token(&self, kind: TokenKind<'a>, line: usize, col: usize) -> Token<'a> {
        Token { kind, line, col }
    }

    fn lex_ident_or_keyword(&mut self) -> Option<Token<'a>> {
        let line = self.line;
        let col = self.col;
        let id = self.eat_while(|x| x.is_ascii_alphabetic() || x == b'_');

        if id.is_empty() {
            None
        } else {
            let keywords = [(b"let", TokenKind::Let)];

            for (s, t) in keywords {
                if s == id {
                    return Some(self.make_token(t, line, col));
                }
            }

            Some(self.make_token(TokenKind::Ident(tostr(id)), line, col))
        }
    }

    fn lex_operator(&mut self) -> Option<Token<'a>> {
        let ops = [(b"=", TokenKind::Equals), (b"+", TokenKind::Plus)];
        let line = self.line;
        let col = self.col;
        for (s, t) in ops {
            if self.eat_str(s) {
                return Some(self.make_token(t, line, col));
            }
        }
        None
    }

    fn lex_numlit(&mut self) -> Option<Token<'a>> {
        let line = self.line;
        let col = self.col;
        let it = self.eat_while(|x| x.is_ascii_digit());
        if it.is_empty() {
            None
        } else {
            Some(self.make_token(TokenKind::NumLit(tostr(it)), line, col))
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Result<Token<'a>, LexError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.consume_whitespace();

        if self.is_empty() {
            return None;
        }

        let lexers = [
            Self::lex_ident_or_keyword,
            Self::lex_operator,
            Self::lex_numlit,
        ];
        for l in lexers {
            if let Some(tok) = l(self) {
                return Some(Ok(tok));
            }
        }

        Some(Err(LexError::Unexpected(tostr(self.src))))
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum LexError<'a> {
    Unexpected(&'a str),
}

pub fn tokenize(src: &str) -> TokenStream {
    TokenStream {
        src: src.as_bytes(),
        line: 1,
        col: 1,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eat_while() {
        let mut ts = tokenize("aaabc");
        let eaten = ts.eat_while(|x| x == b'a');

        assert_eq!(eaten, b"aaa");
        assert_eq!(ts.src, b"bc");
    }

    #[test]
    fn test_kwd_ident_lex() {
        let mut ts = tokenize("lettuce");
        let t = ts.next().unwrap();
        assert_eq!(
            t,
            Ok(Token {
                kind: TokenKind::Ident("lettuce"),
                line: 1,
                col: 1,
            })
        );

        let mut ts = tokenize("let a = b");
        let t = ts.next().unwrap();
        assert_eq!(
            t,
            Ok(Token {
                kind: TokenKind::Let,
                line: 1,
                col: 1,
            })
        );
    }

    #[test]
    fn test_lex_stuff() {
        use TokenKind::*;

        let tokens = tokenize("let add_two a b = a + b")
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token {
                    line: 1,
                    col: 1,
                    kind: Let
                },
                Token {
                    line: 1,
                    col: 5,
                    kind: Ident("add_two")
                },
                Token {
                    line: 1,
                    col: 13,
                    kind: Ident("a")
                },
                Token {
                    line: 1,
                    col: 15,
                    kind: Ident("b")
                },
                Token {
                    line: 1,
                    col: 17,
                    kind: Equals
                },
                Token {
                    line: 1,
                    col: 19,
                    kind: Ident("a")
                },
                Token {
                    line: 1,
                    col: 21,
                    kind: Plus
                },
                Token {
                    line: 1,
                    col: 23,
                    kind: Ident("b")
                }
            ]
        );

        let tokens = tokenize("let foo thing = 1928 + thing")
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token {
                    line: 1,
                    col: 1,
                    kind: Let
                },
                Token {
                    line: 1,
                    col: 5,
                    kind: Ident("foo")
                },
                Token {
                    line: 1,
                    col: 9,
                    kind: Ident("thing")
                },
                Token {
                    line: 1,
                    col: 15,
                    kind: Equals
                },
                Token {
                    line: 1,
                    col: 17,
                    kind: NumLit("1928")
                },
                Token {
                    line: 1,
                    col: 22,
                    kind: Plus
                },
                Token {
                    line: 1,
                    col: 24,
                    kind: Ident("thing")
                }
            ]
        );
    }
}
