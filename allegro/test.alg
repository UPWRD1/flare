op test = (a: Int) -> Int is {
    return a * 3;
}

op main = () -> .. is {
    val x: Int = 2;
    test(x);
}
