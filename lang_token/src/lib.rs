#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TokenKind<'a> {
    Let,
    Ident(&'a str),
    Equals,
    Plus,
    NumLit(&'a str),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Span {
    pub lo_line: usize,
    pub lo_col: usize,
    pub hi_line: usize,
    pub hi_col: usize,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}

pub struct TokenStream<'a> {
    src: &'a [u8],
    line: usize,
    col: usize,
    prev_char_loc: (usize, usize),
}

fn tostr(x: &[u8]) -> &str {
    std::str::from_utf8(x).unwrap()
}

impl<'a> TokenStream<'a> {
    fn is_empty(&self) -> bool {
        self.src.is_empty()
    }

    /// (line, col)
    fn loc(&self) -> (usize, usize) {
        (self.line, self.col)
    }

    fn next_char(&mut self) -> Option<u8> {
        if self.src.is_empty() {
            None
        } else {
            let prev = self.loc();

            let ch = self.src[0];
            if ch == b'\n' {
                self.col = 1;
                self.line += 1;
            } else {
                self.col += 1;
            }
            self.src = &self.src[1..];
            self.prev_char_loc = prev;
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

    /// Makes a token with the given kind, and a span ranging from `(lo_line, lo_col)` to one char before the current loc
    fn make_token_with_lo(
        &self,
        kind: TokenKind<'a>,
        (lo_line, lo_col): (usize, usize),
    ) -> Token<'a> {
        let (hi_line, hi_col) = self.prev_char_loc;

        Token {
            kind,
            span: Span {
                lo_line,
                lo_col,
                hi_line,
                hi_col,
            },
        }
    }

    fn lex_ident_or_keyword(&mut self) -> Option<Token<'a>> {
        let lo = self.loc();
        if let Some(ch) = self.peek_char() {
            if !(ch.is_ascii_alphabetic() || ch == b'_') {
                return None;
            }
        }
        let id = self.eat_while(|x| x.is_ascii_alphanumeric() || x == b'_');

        if id.is_empty() {
            None
        } else {
            let keywords = [(b"let", TokenKind::Let)];

            for (s, t) in keywords {
                if s == id {
                    return Some(self.make_token_with_lo(t, lo));
                }
            }

            Some(self.make_token_with_lo(TokenKind::Ident(tostr(id)), lo))
        }
    }

    fn lex_operator(&mut self) -> Option<Token<'a>> {
        let ops = [(b"=", TokenKind::Equals), (b"+", TokenKind::Plus)];
        let lo = self.loc();
        for (s, t) in ops {
            if self.eat_str(s) {
                return Some(self.make_token_with_lo(t, lo));
            }
        }
        None
    }

    fn lex_numlit(&mut self) -> Option<Token<'a>> {
        let lo = self.loc();
        let it = self.eat_while(|x| x.is_ascii_digit());
        if it.is_empty() {
            None
        } else {
            Some(self.make_token_with_lo(TokenKind::NumLit(tostr(it)), lo))
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
        prev_char_loc: (0, 0),
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
                span: Span {
                    lo_line: 1,
                    hi_line: 1,
                    lo_col: 1,
                    hi_col: 7
                }
            })
        );

        let mut ts = tokenize("let a = b");
        let t = ts.next().unwrap();
        assert_eq!(
            t,
            Ok(Token {
                kind: TokenKind::Let,
                span: Span {
                    lo_line: 1,
                    hi_line: 1,
                    lo_col: 1,
                    hi_col: 3,
                }
            })
        );
    }

    #[test]
    fn test_lex_stuff() {
        use TokenKind::*;

        let tokens = tokenize("let foo123 = 123")
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: Let,
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 1,
                        hi_col: 3,
                    }
                },
                Token {
                    kind: Ident("foo123"),
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 5,
                        hi_col: 10,
                    }
                },
                Token {
                    kind: Equals,
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 12,
                        hi_col: 12,
                    }
                },
                Token {
                    kind: NumLit("123"),
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 14,
                        hi_col: 16,
                    }
                }
            ]
        );

        let tokens = tokenize("let add_two a b = a + b")
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token {
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 1,
                        hi_col: 3,
                    },
                    kind: Let
                },
                Token {
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 5,
                        hi_col: 11,
                    },
                    kind: Ident("add_two")
                },
                Token {
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 13,
                        hi_col: 13,
                    },
                    kind: Ident("a")
                },
                Token {
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 15,
                        hi_col: 15,
                    },
                    kind: Ident("b")
                },
                Token {
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 17,
                        hi_col: 17,
                    },
                    kind: Equals
                },
                Token {
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 19,
                        hi_col: 19,
                    },
                    kind: Ident("a")
                },
                Token {
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 21,
                        hi_col: 21,
                    },
                    kind: Plus
                },
                Token {
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 23,
                        hi_col: 23,
                    },
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
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 1,
                        hi_col: 3,
                    },
                    kind: Let
                },
                Token {
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 5,
                        hi_col: 7,
                    },
                    kind: Ident("foo")
                },
                Token {
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 9,
                        hi_col: 13,
                    },
                    kind: Ident("thing")
                },
                Token {
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 15,
                        hi_col: 15,
                    },
                    kind: Equals
                },
                Token {
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 17,
                        hi_col: 20,
                    },
                    kind: NumLit("1928")
                },
                Token {
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 22,
                        hi_col: 22,
                    },
                    kind: Plus
                },
                Token {
                    span: Span {
                        lo_line: 1,
                        hi_line: 1,
                        lo_col: 24,
                        hi_col: 28,
                    },
                    kind: Ident("thing")
                }
            ]
        );
    }
}
