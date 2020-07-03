extern crate piston_window;
extern crate quickcheck; 
#[macro_use] extern crate quickcheck_derive;

use std::collections::VecDeque;
use rand::{Rng, thread_rng};
use rand::distributions::Uniform;
use piston_window::*;

macro_rules! forall {
    ($i: ident in $range: expr, $predicate: expr) => {
        {
            let mut $i = $range.start; while $i < $range.end && $predicate {$i+=1;};
            $i == $range.end
        }
    };
}

fn main() {
    const REFRESH_RATE: f64 = 1.0/20.0;
    const H: f64 = 20.0;
    const W: f64 = 35.0;
    const SCALE: f64 = 20.0;
    
    // retry game hack
    loop {
        let mut dt: f64 = 0.0;
        let mut s = Snake::new(Point((W/2.0) as i32, (H/2.0) as i32), Point(W as i32, H as i32));

        // window init
        let mut window: PistonWindow =
            WindowSettings::new("The Mighty Snake", [SCALE*W, SCALE*H])
            .exit_on_esc(true).build().unwrap();
        println!("start: {:?}", s);
        println!("game_over {}", s.game_over());
    
        // start game hack
        while let Some(event) = window.next() {
            if let Some(Button::Keyboard(Key::Space)) = event.press_args() {
                break;
            }
        }

        // game loops until game_over
        while let (Some(event), false) = (window.next(), s.game_over()) {

            // input: handle direction
            if let Some(Button::Keyboard(key)) = event.press_args() {
                if let Some(dir) = match key {
                    Key::Left    => Some(Direction::Left),
                    Key::Right   => Some(Direction::Right),
                    Key::Down    => Some(Direction::Up),
                    Key::Up      => Some(Direction::Down),
                    _ => None
                } {
                    //println!("dir: {:?}", dir);
                    s.change_direction(dir);
                    s.advance();
                    dt = 0.0;    
                }
            }       

            // refresh rate handler, timer
            event.update(|arg| {
                dt += arg.dt; // use a real timer, right now more event = more update..
                if dt > REFRESH_RATE {
                    /*println!("in update, dt: {}", dt);
                    println!("game_over {}", s.game_over());
                    println!("food {:?}", s.board.food);
                    println!("snake {:?}", s.body);*/
                    dt = 0.0;
                    s.advance();
                }
            });

            // draw
            window.draw_2d(&event, |context, graphics, _device| {
                clear([0.3; 4], graphics);

                // snake
                for &Point(x, y) in &s.body {   
                    rectangle([1.0, 0.0, 0.0, 1.0], // red
                        [SCALE*x as f64, SCALE*y as f64, SCALE, SCALE],
                        context.transform,
                        graphics);
                }

                // food
                let Point(food_x, food_y) = s.board.food;
                rectangle([0.0, 1.0, 0.0, 1.0], // green
                    [SCALE*food_x as f64, SCALE*food_y as f64, SCALE, SCALE],
                    context.transform,
                    graphics);
            });
        }
        println!("snake {:?}", s.body);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Arbitrary, Copy)]
struct Point(i32, i32);
impl Point {
    #[cfg(test)]
    fn norm_l1(&self, b: Point) -> i32 {
        (self.0-b.0).abs()+(self.1-b.1).abs()
    }

    fn random(bot_left: Point, top_right: Point) -> Point {   // todo real uniform generator
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

    fn new(top_right: Point) -> Self {
        let mut b = Board {top_right: top_right, food: Point(-1, -1)};
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

    fn new(start: Point, top_right: Point) -> Self {
        Snake {
            board: Board::new(top_right),
            body: VecDeque::from(vec![start]),
            dir: Direction::Left,
            game_over: false,
        }
    }

    pub fn advance(&mut self) -> &mut Self { 
        self.grow();     
        let snake_ate_food = self.body[0] == self.board.food;  
        if snake_ate_food {
            self.board.replace_food();
        } else {
            self.shrink();
        }
        self
    }

    pub fn change_direction(&mut self, dir: Direction) -> &mut Self {
        let does_not_go_backward = dir!=self.dir.behind();
        if does_not_go_backward {
            self.dir = dir;
        }
        self
    }

    pub fn game_over(&self) -> bool {
        self.game_over
    }

    fn grow(&mut self) -> &mut Self {
        let head = self.body[0];
        let new_head = self.dir.advance(head);
        self.body.push_front(new_head);
        if !self.head_in_legal_state() {
            self.game_over = true;
            // TODO destroy the body to cause exceptions if game_over is not handled ?
        }
        self
    }

    fn shrink(&mut self) -> &mut Self {
        self.body.pop_back();
        self
    }

    fn head_in_legal_state(&self) -> bool {
        let head = self.body[0];
        let did_not_bit_itself = forall!(i in 1..self.body.len(), head!=self.body[i]);
        self.board.contains(head) && did_not_bit_itself              
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
        let mut s = Snake::new(Point(0, 0), Point(100, 100));
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
        let b = Board::new(Point(100, 100));
        assert!(b.contains(Point(50, 50)));
        assert!(b.contains(Point(0, 34)));
        assert!(b.contains(Point(0, 0)));

        assert!(!b.contains(Point(100, 50)));
        assert!(!b.contains(Point(-1, 50)));
        assert!(!b.contains(Point(5, 102)));

        // in legal state
        s = Snake::new(Point(0, 0), Point(100, 100));
        s.board.food = Point(1000, 1000);
        assert_eq!(s.dir, Direction::Left);
        assert!(s.head_in_legal_state());
        assert!(!s.game_over());

        s.grow();
        assert_eq!(s.body[0], Point(-1, 0));
        assert!(!s.head_in_legal_state());
        assert!(s.game_over());

        s.change_direction(Direction::Up).advance().change_direction(Direction::Right).advance();
        assert_eq!(s.body[0], Point(0, 1));
        assert!(s.head_in_legal_state());
        assert!(s.game_over());

        s.change_direction(Direction::Down).advance().advance();
        assert_eq!(s.body[0], Point(0, -1));
        assert!(!s.head_in_legal_state());

        s = Snake::new(Point(10, 10), Point(100, 100)); 
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

            let mut s = Snake::new(Point(0, 0), Point(100, 100));
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
                            assert_eq!(old_lst, new_lst, "Snake tail does not change when it eats food");
                        } else {
                            assert_ne!(old_lst, new_lst, "Snake tail changes when it advances and does not eat food");
                        }
                        assert_ne!(old_fst, new_fst, "Snake head changes when it advances");
                    }
                }
                let all_neighbors = forall!(i in 1..s.body.len(), 1==s.body[i-1].norm_l1(s.body[i]));                
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

