-- we love comments

op test: int  (y: int) -> return y;

op main: int of () -> {
    let x := test(1);
    return x;
}
