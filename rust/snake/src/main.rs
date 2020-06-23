extern crate quickcheck; 
#[macro_use] extern crate quickcheck_derive;

use std::collections::VecDeque;
use rand::{Rng, thread_rng};
use rand::distributions::Uniform;

fn main() {
    println!("Hello, world!");
}

#[derive(Debug, PartialEq, Eq, Clone, Arbitrary, Copy)]
struct Point(i32, i32);
impl Point {
    fn norm_l1(&self, b: Point) -> i32 {
        (self.0-b.0).abs()+(self.1-b.1).abs()
    }

    fn random(bot_left: Point, top_right: Point) -> Point {   
        let mut rng = thread_rng(); // todo static lifetime ?
        let x_range = Uniform::new(bot_left.0, top_right.0);
        let y_range = Uniform::new(bot_left.1, top_right.1);
        Point(rng.sample(x_range),rng.sample(y_range))
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
struct Board{top_right: Point, food: Point}
impl Board {

    fn new() -> Self {
        let mut b = Board {top_right: Point(100, 100), food: Point(-1, -1)};
        b.replace_food();
        b
    }

    fn contains(&self, p: Point) -> bool {
        (0..self.top_right.0).contains(&p.0) &&        
        (0..self.top_right.1).contains(&p.1)
    }

    fn replace_food(&mut self) {
        self.food = Point::random(Point(0, 0), self.top_right);
    }
}

#[derive(Debug)]
struct Snake {
    board: Board,
    body: VecDeque<Point>,
    dir: Direction,
    game_over: bool,
}
impl Snake {

    fn new(start: Point) -> Self {
        Snake {
            board: Board::new(),
            body: VecDeque::from(vec![start]),
            dir: Direction::Left,
            game_over: false,
        }
    }

    pub fn advance(&mut self) -> &mut Self { 
        self.grow();       
        if self.body[0] == self.board.food {
            self.board.replace_food();
        } else {
            self.body.pop_back();
        }
        self
    }

    pub fn change_direction(&mut self, dir: Direction) -> &mut Self {
        if dir!=self.dir.behind() {
            self.dir = dir;
        }
        self
    }

    pub fn is_game_over(&self) -> bool {
        self.game_over
    }

    fn grow(&mut self) -> &mut Self {
        let fst = self.body.front().unwrap().clone();
        let new_fst = self.dir.advance(fst);
        self.body.push_front(new_fst);
        if !self.head_in_legal_state() {
            self.game_over = true;
            // TODO destroy the body to cause exceptions if game_over is not handled ?
        }
        self
    }

    fn head_in_legal_state(&self) -> bool {
        let head_not_in_body = { // while_all macro to represent this construct ? / all <predicate> for <var> in <range>
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
    use super::*;
    use super::quickcheck::QuickCheck;
    
    #[test]
    fn basics() {
        let mut s = Snake::new(Point(0, 0));
        s.board.food = Point(1000, 1000);

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

        s.board.food = Point(0, 1);
        s.advance();
        assert_eq!(s.body, VecDeque::from(vec![Point(0, 1), Point(-1, 1), Point(-2, 1), Point(-2, 0), Point(-1, 0)]));
        
        s.change_direction(s.dir.behind());
        assert_eq!(s.dir, Direction::Right);

        println!("Snake: {:?}", s);

        // board contains
        let b = Board::new();
        assert!(b.contains(Point(50, 50)));
        assert!(b.contains(Point(0, 34)));
        assert!(b.contains(Point(0, 0)));

        assert!(!b.contains(Point(100, 50)));
        assert!(!b.contains(Point(-1, 50)));
        assert!(!b.contains(Point(5, 102)));

        // in legal state
        s = Snake::new(Point(0, 0));
        s.board.food = Point(1000, 1000);
        assert_eq!(s.dir, Direction::Left);
        assert!(s.head_in_legal_state());
        assert!(!s.is_game_over());

        s.grow();
        assert_eq!(s.body[0], Point(-1, 0));
        assert!(!s.head_in_legal_state());
        assert!(s.is_game_over());

        s.change_direction(Direction::Up).advance().change_direction(Direction::Right).advance();
        assert_eq!(s.body[0], Point(0, 1));
        assert!(s.head_in_legal_state());
        assert!(s.is_game_over());

        s.change_direction(Direction::Down).advance().advance();
        assert_eq!(s.body[0], Point(0, -1));
        assert!(!s.head_in_legal_state());

        s = Snake::new(Point(10, 10)); 
        s.board.food = Point(1000, 1000);
        assert!(s.head_in_legal_state());
        s.grow().change_direction(Direction::Up).grow().change_direction(Direction::Right).grow().change_direction(Direction::Down).grow();
        assert_eq!(s.body[0], Point(10, 10));
        assert!(!s.head_in_legal_state(), "Snake bit itself");
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

            let mut s = Snake::new(Point(0, 0));
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
                        let food = s.board.food;
                        let (old_fst, old_lst) = (s.body[0], s.body.back().cloned());
                        s.advance();
                        let (new_fst, new_lst) = (s.body[0], s.body.back().cloned());                        
                        if s.body[0]==food {
                            m.len += 1;
                            assert_eq!(old_lst, new_lst);
                        } else {
                            assert_ne!(old_lst, new_lst, "Snake tail changes when it advances and does not eat food");
                        }
                        assert_ne!(old_fst, new_fst, "Snake head changes when it advances");
                    }
                }
                //&s.body[0..(s.body.len()-1)].zip(&s.body[1..]).all(|a, b| Point::norm_l1(a, b)==1); TODO figure out slices of vecdeque                
                let all_neighbors = {
                    let mut i = 1; while i < s.body.len() && 1==s.body[i-1].norm_l1(s.body[i]) {
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

