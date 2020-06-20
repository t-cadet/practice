extern crate quickcheck; 
#[macro_use] extern crate quickcheck_derive;

use std::collections::VecDeque;

fn main() {
    println!("Hello, world!");
}

#[derive(Debug, PartialEq, Eq, Clone, Arbitrary)]
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

    pub fn shrink(&mut self) -> Option<Point> {
        self.body.pop_back()
    }

    pub fn grow(&mut self, value: Point) {
        self.body.push_front(value);
    }
}

// to access the tests output:
// cargo test -- --nocapture
#[cfg(test)]
mod test {
    use super::Snake;
    use super::Point;
    use super::quickcheck::QuickCheck;
    
    #[test]
    fn basics() {
        let mut s = Snake::new();

        assert_eq!(s.shrink(), None);

        s.grow(Point(1, 3));
        s.grow(Point(1, 2));
        s.grow(Point(1, 1));

        assert_eq!(s.shrink(), Some(Point(1,3)));
        assert_eq!(s.shrink(), Some(Point(1,2)));

        s.grow(Point(1, 0));

        assert_eq!(s.shrink(), Some(Point(1,1)));
        assert_eq!(s.shrink(), Some(Point(1,0)));
        assert_eq!(s.shrink(), None);
    }

    #[test]
    fn model_based_testing() {
        #[derive(Debug, Clone, Arbitrary)]
        enum Op {
            Grow(Point),
            Shrink,
        }

        struct SnakeModel {
            len: usize,
        }

        struct Stats {
            sum_len: usize,
            n_tests: usize,
            sample: Vec<Op>
        }
        static mut STATS:Stats = Stats{sum_len: 0, n_tests: 0, sample: Vec::new()};

        fn p(ops: Vec<Op>) -> bool {
            unsafe {
                STATS.sum_len += ops.len();
                STATS.n_tests += 1;
                if STATS.sample.is_empty() {
                    STATS.sample = ops.clone();
                }
            }

            let mut s = Snake::new();
            let mut m = SnakeModel{len: 0};

            for op in ops {
                match op {
                    Op::Grow(p) => {
                        s.grow(p);
                        m.len += 1;
                    }
                    Op::Shrink => {
                        // s.shrink() == last TODO
                        s.shrink();
                        m.len = if m.len==0 {0} else {m.len-1};
                    }
                }
                assert_eq!(m.len, s.body.len());    
            }
            true
        }
        QuickCheck::new().quickcheck(p as fn(Vec<Op>) -> bool);
        unsafe{            
            println!("{} Ops by test on average", {STATS.sum_len/STATS.n_tests});
            println!("Sample Vec<Op>: {:?}", {&STATS.sample});
        }
    }
}

