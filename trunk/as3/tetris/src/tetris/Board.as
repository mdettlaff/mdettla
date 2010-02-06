package tetris {

    import flash.display.Sprite;
    import flash.geom.Rectangle;

    public class Board extends Sprite {

        public static const WIDTH:int = 10;
        public static const HEIGHT:int = 20;
        public static const BLOCK_SIZE:int = 15;

        private var board:Array;

        private var bounds:Rectangle;
        private var lineColor:Number;
        private var fillColor:Number;

        public function Board(fillColor:Number = 0xFFFFFF,
                lineColor:Number = 0x000000) {
            this.bounds = new Rectangle(0, 0,
                    WIDTH * BLOCK_SIZE, HEIGHT * BLOCK_SIZE);
            this.fillColor = fillColor;
            this.lineColor = lineColor;
            this.x = 5;
            this.y = 5;
            this.board = Utils.createArray2D(WIDTH, HEIGHT);
            drawBounds();
        }

        public function isConflictWith(t:Tetromino):Boolean {
            for (var i:int = 0; i < t.size; i++) {
                for (var j:int = 0; j < t.size; j++) {
                    if (t.shape[i][j]) {
                        if (t.xCoord + i < 0 || t.xCoord + i > Board.WIDTH - 1
                                || t.yCoord + j > Board.HEIGHT - 1
                                || board[t.xCoord + i][t.yCoord + j]) {
                            return true;
                        }
                    }
                }
            }
            return false;
        }

        private function drawBounds():void {
            this.graphics.clear();
            this.graphics.lineStyle(1.0, this.lineColor, 1.0);
            this.graphics.beginFill(this.fillColor, 1.0);
            this.graphics.drawRect(
                    bounds.left - 1, bounds.top = 1,
                    bounds.width + 2, bounds.height + 2);
            this.graphics.endFill();
        }
    }
}
