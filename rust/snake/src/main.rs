use std::collections::VecDeque;

fn main() {
    println!("Hello, world!");
}

#[derive(Debug, PartialEq, Eq)]
struct Point(i32, i32);

#[derive(Debug)]
struct Snake {
    body: VecDeque<Point>
}

impl Snake {

    fn new() -> Self {
        Snake{body: VecDeque::new()}
    }

    pub fn show(&self) {
        println!("Snake: {:?}", self)
    }

    pub fn pop_back(&mut self) -> Option<Point> {
        self.body.pop_back()
    }

    pub fn push_front(&mut self, value: Point) {
        self.body.push_front(value);
    }
}

#[cfg(test)]
mod test {
    use super::Snake;
    use super::Point;
    #[test]
    fn basics() {
        let mut s = Snake::new();

        assert_eq!(s.pop_back(), None);

        s.push_front(Point(1, 3));
        s.push_front(Point(1, 2));
        s.push_front(Point(1, 1));

        assert_eq!(s.pop_back(), Some(Point(1,3)));
        assert_eq!(s.pop_back(), Some(Point(1,2)));

        s.push_front(Point(1, 0));

        assert_eq!(s.pop_back(), Some(Point(1,1)));
        assert_eq!(s.pop_back(), Some(Point(1,0)));
        assert_eq!(s.pop_back(), None);
    }
}

