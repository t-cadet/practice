extern crate quickcheck; 
#[macro_use] extern crate quickcheck_derive;

use std::collections::VecDeque;

fn main() {
    println!("Hello, world!");
}

#[derive(Debug, PartialEq, Eq, Clone, Arbitrary, Copy)]
struct Point(i32, i32);

impl Point {
    fn norm_L1(&self, b: Point) -> i32 {
        (self.0-b.0).abs()+(self.1-b.1).abs()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Arbitrary)]
enum Direction { Left, Right, Down, Up }

impl Direction {
    fn advance(&self, mut p: Point) -> Point {
        match &self {
            Self::Left    => p.0 -= 1,
            Self::Right   => p.0 += 1,
            Self::Down    => p.1 -= 1,
            Self::Up      => p.1 += 1,
        }
        p
    }

    fn behind(&self) -> Self {
        match &self {
            Self::Left    => Self::Right,
            Self::Right   => Self::Left,
            Self::Down    => Self::Up,
            Self::Up      => Self::Down
        }
    }
}

#[derive(Debug)]
struct Board{top_right: Point}
impl Board {

    fn new() -> Self {
        Board {top_right: Point(100, 100)}
    }

    fn contains(&self, p: Point) -> bool {
        (0..self.top_right.0).contains(&p.0) &&        
        (0..self.top_right.1).contains(&p.1)
    }
}

#[derive(Debug)]
struct Snake {
    board: Board,
    body: VecDeque<Point>,
    dir: Direction
}

impl Snake {

    fn new() -> Self {
        Snake {
            board: Board::new(),
            body: VecDeque::from(vec![Point(0, 0)]),
            dir: Direction::Left
        }
    }

    pub fn advance(&mut self) -> &mut Self { 
        self.grow();       
        self.body.pop_back();
        self
    }

    pub fn change_direction(&mut self, dir: Direction) -> &mut Self {
        if dir!=self.dir.behind() {
            self.dir = dir;
        }
        self
    }

    pub fn grow(&mut self) -> &mut Self {
        let fst = self.body.front().unwrap().clone();
        let new_fst = self.dir.advance(fst);
        self.body.push_front(new_fst);
        self
    }

    pub fn in_legal_state(&self) -> bool {
        let head_not_in_body = {
            let mut i = 1; while i < self.body.len() && self.body[0]!=self.body[i] {
                i +=1;
            };
            i == self.body.len()
        };
        self.board.contains(self.body[0]) && head_not_in_body              
    }
}

// to access the tests output:
// cargo test -- --nocapture
#[cfg(test)]
mod test {
    use super::Direction;
    use super::Point;
    use super::quickcheck::QuickCheck;
    use super::Snake;
    use super::VecDeque;
    
    #[test]
    fn basics() {
        let mut s = Snake::new();

        s.grow().grow();
        assert_eq!(s.body, VecDeque::from(vec![Point(-2, 0), Point(-1, 0), Point(0, 0)]));

        s.change_direction(Direction::Up);
        assert_eq!(s.dir, Direction::Up);

        s.change_direction(Direction::Down);
        assert_eq!(s.dir, Direction::Up);

        s.grow();
        assert_eq!(s.body, VecDeque::from(vec![Point(-2, 1), Point(-2, 0), Point(-1, 0), Point(0, 0)]));
        
        s.change_direction(Direction::Right).advance();
        assert_eq!(s.body, VecDeque::from(vec![Point(-1, 1), Point(-2, 1), Point(-2, 0), Point(-1, 0)]));
        
        s.change_direction(s.dir.behind());
        assert_eq!(s.dir, Direction::Right);

        println!("Snake: {:?}", s);
    }

    #[test]
    fn model_based_testing() {
        #[derive(Debug, Clone, Arbitrary)]
        enum Op {
            Advance,
            ChangeDirection(Direction),
            Grow,
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
            let mut m = SnakeModel{len: 1};

            for op in ops {
                match op {
                    Op::Grow => {
                        s.grow();
                        m.len += 1;
                    }
                    Op::ChangeDirection(dir) => {
                        s.change_direction(dir);
                    }
                    Op::Advance => {
                        let old_fst_lst = (s.body.front().cloned(), s.body.back().cloned());
                        s.advance();
                        let new_fst_lst = (s.body.front().cloned(), s.body.back().cloned());
                        assert_ne!(old_fst_lst, new_fst_lst, "Snake extremities change when it advances");
                    }
                }
                //&s.body[0..(s.body.len()-1)].zip(&s.body[1..]).all(|a, b| Point::norm_L1(a, b)==1); TODO figure out slices of vecdeque                
                let all_neighbors = {
                    let mut i = 1; while i < s.body.len() && 1==s.body[i-1].norm_L1(s.body[i]) {
                        i+=1;
                    };
                    i == s.body.len()
                };
                assert!(all_neighbors, "All points of snake are neighbors");
                assert_eq!(m.len, s.body.len(), "Snake length is consistent with the model");    
            }
            true
        }
        QuickCheck::new().quickcheck(p as fn(Vec<Op>) -> bool);
        unsafe{            
            println!("{} tests ", STATS.n_tests);
            println!("{} Ops by test on average", {STATS.sum_len/STATS.n_tests});
            println!("Sample Vec<Op>: {:?}", {&STATS.sample});
        }
    }
}

