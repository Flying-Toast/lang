#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Token<'a> {
    Let,
    Ident(&'a str),
    Equals,
    Plus,
    NumLit(&'a str),
}

pub struct TokenStream<'a> {
    src: &'a [u8],
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
            self.src = &self.src[1..];
            Some(ch)
        }
    }

    fn peek_char(&self) -> Option<u8> {
        self.src.first().copied()
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

    fn lex_ident_or_keyword(&mut self) -> Option<Token<'a>> {
        let id = self.eat_while(|x| x.is_ascii_alphabetic() || x == b'_');

        if id.is_empty() {
            None
        } else {
            let keywords = [(b"let", Token::Let)];

            for (s, t) in keywords {
                if s == id {
                    return Some(t);
                }
            }

            Some(Token::Ident(tostr(id)))
        }
    }

    fn lex_operator(&mut self) -> Option<Token<'a>> {
        let ops = [(b"=", Token::Equals), (b"+", Token::Plus)];
        for (s, t) in ops {
            if self.src.starts_with(s) {
                self.src = &self.src[s.len()..];
                return Some(t);
            }
        }
        None
    }

    fn lex_numlit(&mut self) -> Option<Token<'a>> {
        let it = self.eat_while(|x| x.is_ascii_digit());
        if it.is_empty() {
            None
        } else {
            Some(Token::NumLit(tostr(it)))
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
        assert_eq!(t, Ok(Token::Ident("lettuce")));

        let mut ts = tokenize("let a = b");
        let t = ts.next().unwrap();
        assert_eq!(t, Ok(Token::Let));
    }

    #[test]
    fn test_lex_stuff() {
        use Token::*;

        let tokens = tokenize("let add_two a b = a + b")
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Let,
                Ident("add_two"),
                Ident("a"),
                Ident("b"),
                Equals,
                Ident("a"),
                Plus,
                Ident("b")
            ]
        );

        let tokens = tokenize("let foo thing = 1928 + thing")
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Let,
                Ident("foo"),
                Ident("thing"),
                Equals,
                NumLit("1928"),
                Plus,
                Ident("thing")
            ]
        );
    }
}
