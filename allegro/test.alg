-- we love comments 

let printXNumTimes: .. of (x: str, times: int) -> {
    var current: int = 0;
    while current < times {
        print x;
        current = current + 1
    }
}

let main: .. of () -> {
    printXNumTimes("Hello world!", 4)
}
