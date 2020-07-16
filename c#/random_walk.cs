using System;

static class MainClass {

  /* Mathematical modulo (-2 % 3 == 1) */
  public static int Mod(int a, int b) =>
    (a + b) % b;

  public static string ReplaceAt(this string str, int index, int length, string replace) =>
    str.Remove(index, Math.Min(length, str.Length - index)).Insert(index, replace);

  public class Direction {
    public static Position LEFT = new Position(-1, 0);
    public static Position UP = new Position(0, 1);
    public static Position RIGHT = new Position(1, 0);
    public static Position DOWN = new Position(0, -1);

    public static Position GetRandom(Random gen) {
      switch (gen.Next(4)) { // TODO use new switch
        case 0: return LEFT;
        case 1: return UP;
        case 2: return RIGHT;
        case 3: return DOWN;
      };
      throw new Exception("Direction.GetRandom is broken");
    }
  }

  // TODO make this an immutable record
  public class Position {
    public int x {get;}
    public int y {get;}

    public Position(int x, int y) {
      this.x = x;
      this.y = y;
    }

    public static Position GetRandom(Random gen, int dimension) =>
      new Position(gen.Next(dimension), gen.Next(dimension));

    public Position Add(Position p) =>
      new Position(x + p.x, y + p.y);

    public Position Apply(Func<int, int> f) =>
      new Position(f(x), f(y));

    public Position GetRandomNeighbor(Random gen) =>
      Add(Direction.GetRandom(gen));
      
  }

  public class RandomWalk {
    public bool[,] board;
    Random generator;
    Position lastPosition;
    public int stepCounter {get; private set;}   

    public RandomWalk(int dimension) {
      board = new bool[dimension, dimension];
      generator = new Random();
      lastPosition = Position.GetRandom(generator, dimension);
      stepCounter = 0;
      UpdateBoard();
    }

    void UpdateBoard() {
      board[lastPosition.y, lastPosition.x] = true;
    }

    public string BoardToString() {
      Func<bool, char> BoolToSymbol = b =>
        b ? '・' : '〒';
      Func<string, string> SetDifferentSymbolForLastPosition = s =>
        s.ReplaceAt(lastPosition.x + lastPosition.y*(1+(board.GetLength(0))), 1, "〠");

      string str = "";
      for(int i=0; i<board.GetLength(0); i++) {
        for(int j=0; j<board.GetLength(1); j++)
          str += BoolToSymbol(board[i, j]);
        str+="\n";
      }
      return SetDifferentSymbolForLastPosition(str);
    }

    Position WrapOverflow(Position p) =>
      p.Apply(a => Mod(a, board.GetLength(0)));

    public RandomWalk Walk() { 
      lastPosition = WrapOverflow(lastPosition.GetRandomNeighbor(generator));
      UpdateBoard();
      stepCounter++;
      return this;
    }

    //TODO dictionary GetStats

  }
  public static void Main (string[] args) {    
    ManualWalk(21);
  }

  public static void AutoWalk(int dimension, int refreshRate) {
  }

  public static void ManualWalk(int dimension) {
    var rw = new RandomWalk(dimension);
    string key = "";

    do {
      Console.Clear();
      Console.WriteLine(rw.BoardToString());
      Console.WriteLine ("Press 'Enter' to walk or input 'q' to quit");
      key = Console.ReadLine();
      rw.Walk();
    } while(key!="q");
    Console.WriteLine("You walked " + rw.stepCounter + " steps.");
  }

  //TODO stats to string

  public static void BasicTests() {
    var rw = new RandomWalk(20);
    Console.WriteLine(rw.BoardToString());
    
    rw.Walk().Walk().Walk();
    Console.WriteLine(rw.BoardToString());
  }
}