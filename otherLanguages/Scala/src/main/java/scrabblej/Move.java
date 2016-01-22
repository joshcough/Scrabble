package scrabblej;


import java.util.List;

class Move {

  private final int value;
  private final boolean bingo;
  private final List<Tile> tiles;

  public Move(List<Tile> tiles) {
    this.tiles = tiles;
    this.value = sumTiles(tiles);
    this.bingo = tiles.size() == 7;
  }

  public int getScore(){ return value; }
  public boolean wasBingo(){ return bingo; }
  public String toString(){
    return "Move(value: " + value + ", bingo: " + bingo + ")";
  }

  private int sumTiles(List<Tile> tiles) {
    int total = 0;
    for(Tile t: tiles) {
      total = total + t.getValue();
    }
    return total;
  }
}
