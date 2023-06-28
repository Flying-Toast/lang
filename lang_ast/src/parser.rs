use crate::*;
use lang_token::{Token, TokenKind as TK, TokenStream};

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum ParseError<'a> {
    Lex(lang_token::LexError<'a>),
    UnexpectedToken(Token<'a>),
    UnexpectedEof,
}

impl<'a> From<lang_token::LexError<'a>> for ParseError<'a> {
    fn from(value: lang_token::LexError<'a>) -> Self {
        Self::Lex(value)
    }
}

pub fn parse(ts: TokenStream) -> Result<Vec<LetBind>, ParseError> {
    let mut p = Parser { ts: ts.peekable() };
    let mut ret = Vec::new();

    while let Some(b) = p.maybe_parse_let_bind()? {
        ret.push(b);
    }

    Ok(ret)
}

struct Parser<'a> {
    ts: std::iter::Peekable<TokenStream<'a>>,
}

impl<'a> Parser<'a> {
    fn expect_next_token(&mut self) -> Result<Token<'a>, ParseError<'a>> {
        Ok(self.ts.next().ok_or(ParseError::UnexpectedEof)??)
    }

    fn maybe_next_token(&mut self) -> Result<Option<Token<'a>>, ParseError<'a>> {
        if self.peek_token()?.is_none() {
            Ok(None)
        } else {
            Ok(Some(self.expect_next_token().unwrap()))
        }
    }

    fn peek_token(&mut self) -> Result<Option<Token<'a>>, ParseError<'a>> {
        if let Some(peeked) = self.ts.peek().copied() {
            Ok(Some(peeked?))
        } else {
            Ok(None)
        }
    }

    fn parse_simple_expr(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        let next = self.expect_next_token()?;

        Ok(match next.kind {
            TK::NumLit(numstring) => Expr::IntLit(IntLitExpr {
                value: numstring.parse().unwrap(),
            }),
            TK::Ident(id) => Expr::BindRef(Ident { name: id }),
            _ => return Err(ParseError::UnexpectedToken(next)),
        })
    }

    fn maybe_parse_binary_expr(
        &mut self,
        lhs: Expr<'a>,
    ) -> Result<Result<Expr<'a>, Expr<'a>>, ParseError<'a>> {
        match self.peek_token()?.map(|x| x.kind) {
            Some(TK::Plus) => {}
            _ => return Ok(Err(lhs)),
        }

        Ok(Ok(match self.expect_next_token().unwrap().kind {
            TK::Plus => Expr::Add(lhs.into(), self.parse_simple_expr()?.into()),
            _ => unreachable!(),
        }))
    }

    fn parse_expr(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        let mut ret = self.parse_simple_expr()?;

        loop {
            match self.maybe_parse_binary_expr(ret)? {
                Err(unchanged) => return Ok(unchanged),
                Ok(wrapped) => ret = wrapped,
            }
        }
    }

    fn maybe_parse_let_bind(&mut self) -> Result<Option<LetBind<'a>>, ParseError<'a>> {
        let lettok = self.maybe_next_token()?;
        let lettok = match lettok {
            None => return Ok(None),
            Some(x) => x,
        };

        if let TK::Let = lettok.kind {
            let bindidenttok = self.expect_next_token()?;

            let ident = match bindidenttok.kind {
                TK::Ident(id) => id,
                _ => return Err(ParseError::UnexpectedToken(bindidenttok)),
            };

            let eqtok = self.expect_next_token()?;
            match eqtok.kind {
                TK::Equals => {}
                _ => return Err(ParseError::UnexpectedToken(eqtok)),
            };

            let value = self.parse_expr()?;

            Ok(Some(LetBind {
                ident: Ident { name: ident },
                value,
            }))
        } else {
            Err(ParseError::UnexpectedToken(lettok))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::Expr::*;

    fn parser(x: &str) -> Parser {
        Parser {
            ts: lang_token::tokenize(x).peekable(),
        }
    }

    fn parsed(x: &str) -> Result<Vec<LetBind>, ParseError> {
        parse(lang_token::tokenize(x))
    }

    #[test]
    fn test_precidence() {
        assert_eq!(
            parser("abc123def + foo + 123 + bar + baz").parse_expr(),
            Ok(Add(
                Add(
                    Add(
                        Add(
                            BindRef(Ident { name: "abc123def" }).into(),
                            BindRef(Ident { name: "foo" }).into()
                        )
                        .into(),
                        IntLit(IntLitExpr { value: 123 }).into()
                    )
                    .into(),
                    BindRef(Ident { name: "bar" }).into()
                )
                .into(),
                BindRef(Ident { name: "baz" }).into()
            ))
        );
    }

    #[test]
    fn test_parse_let_binding() {
        assert_eq!(
            parsed("let foo = 12 + bar"),
            Ok(vec![LetBind {
                ident: Ident { name: "foo" },
                value: Expr::Add(
                    Expr::IntLit(IntLitExpr { value: 12 }).into(),
                    Expr::BindRef(Ident { name: "bar" }).into()
                )
            }])
        );
    }

    #[test]
    fn test_parse_multiple_let_bindings() {
        assert_eq!(
            parsed("let a = foo + bar\nlet other = hello let six=6"),
            Ok(vec![
                LetBind {
                    ident: Ident { name: "a" },
                    value: Expr::Add(
                        Expr::BindRef(Ident { name: "foo" }).into(),
                        Expr::BindRef(Ident { name: "bar" }).into()
                    )
                },
                LetBind {
                    ident: Ident { name: "other" },
                    value: Expr::BindRef(Ident { name: "hello" })
                },
                LetBind {
                    ident: Ident { name: "six" },
                    value: Expr::IntLit(IntLitExpr { value: 6 })
                },
            ])
        );
    }
}
