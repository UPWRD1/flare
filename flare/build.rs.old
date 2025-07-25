use lrlex::CTLexerBuilder;

use cfgrammar::yacc::YaccKind;

fn main() {
    CTLexerBuilder::new()
        .lrpar_config(|ctp| {
            ctp.yacckind(YaccKind::Grmtools).grammar_in_src_dir("flare.y")
                .unwrap()
        })
        .lexer_in_src_dir("flare.l")
        .unwrap()
        .build()
        .unwrap();
}
