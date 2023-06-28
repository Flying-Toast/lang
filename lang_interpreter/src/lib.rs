use lang_ast as ast;

#[derive(Debug)]
pub struct Interpreter<'a> {
    bindings: Vec<ast::LetBind<'a>>,
}

impl<'a> Interpreter<'a> {
    pub fn new_with_bindings(bindings: Vec<ast::LetBind<'a>>) -> Self {
        Self { bindings }
    }

    pub fn eval_binding(&self, name: &str) -> i64 {
        let binding = self
            .get_binding(name)
            .expect(&format!("No binding `{name}`"));

        self.eval(&binding.value)
    }

    fn get_binding(&self, name: &str) -> Option<&ast::LetBind> {
        self.bindings.iter().find(|x| x.ident.name == name)
    }

    fn eval(&self, expr: &ast::Expr) -> i64 {
        match expr {
            ast::Expr::IntLit(ast::IntLitExpr { value }) => *value,
            ast::Expr::BindRef(ast::Ident { name }) => self.eval_binding(name),
            ast::Expr::Add(a, b) => self.eval(a) + self.eval(b),
        }
    }
}
