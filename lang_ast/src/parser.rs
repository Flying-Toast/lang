use crate::expr::*;
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

pub fn parse(ts: TokenStream) -> Result<Expr, ParseError> {
    Parser { ts: ts.peekable() }.parse_expr()
}

struct Parser<'a> {
    ts: std::iter::Peekable<TokenStream<'a>>,
}

impl<'a> Parser<'a> {
    fn expect_next_token(&mut self) -> Result<Token<'a>, ParseError<'a>> {
        Ok(self.ts.next().ok_or(ParseError::UnexpectedEof)??)
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
            TK::Ident(id) => Expr::BindRef(Ident { ident: id }),
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

    #[test]
    fn parser_test() {
        assert_eq!(
            parser("12").parse_expr(),
            Ok(IntLit(IntLitExpr { value: 12 }))
        );

        assert_eq!(
            parser("abc123def + foo + 123 + bar + baz").parse_expr(),
            Ok(Add(
                Add(
                    Add(
                        Add(
                            BindRef(Ident { ident: "abc123def" }).into(),
                            BindRef(Ident { ident: "foo" }).into()
                        )
                        .into(),
                        IntLit(IntLitExpr { value: 123 }).into()
                    )
                    .into(),
                    BindRef(Ident { ident: "bar" }).into()
                )
                .into(),
                BindRef(Ident { ident: "baz" }).into()
            ))
        );
    }
}
