use lang_ast as ast;;
use lang_interpreter as interpreter;
use lang_token as token;

const SOURCE: &'static str = "
let foo = 123
let nine = 9
let twoplustwo = 2 + 2
let fourplusnine = twoplustwo + nine
";

fn main() {
    let bindings = ast::parse(token::tokenize(SOURCE)).unwrap();

    let i = interpreter::Interpreter::new_with_bindings(bindings);

    let mut s = String::new();
    std::io::stdin().read_line(&mut s);
    let s = s.trim();
    println!("`{s}`: {:?}", i.eval_binding(&s));
}
