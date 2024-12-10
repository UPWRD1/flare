type Container = struct of
    capacity: int,
    contents: int

def Container =
    let new of cap: int =
        Container {capacity: cap, contents: 0}

    let get_remaining of self =
        return self.capacity - self.contents
    
    let add of self, wt: int -> Container =
        if self.contents + wt <= self.capacity then 
            Container {capacity: self.capacity, contents: self.contents + wt}
        else 
            Container {capacity: self.capacity * 2, contents: self.contents + wt}
    
end