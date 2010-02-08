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
            this.fillColor = fillColor;
            this.lineColor = lineColor;
            bounds = new Rectangle(0, 0,
                    WIDTH * BLOCK_SIZE, HEIGHT * BLOCK_SIZE);
            x = 5;
            y = 5;
            board = Utils.createArray2D(WIDTH, HEIGHT);
            drawBounds();

            addEventListener(TetrisEvent.TETROMINO_LANDED, landingHandler);
        }

        public function isConflictWithTetrominoState(
                shape:Array, size:int, xCoord:int, yCoord:int):Boolean {
            for (var i:int = 0; i < size; i++) {
                for (var j:int = 0; j < size; j++) {
                    if (shape[i][j]) {
                        if (xCoord + j < 0 || xCoord + j > Board.WIDTH - 1
                                || yCoord + i > Board.HEIGHT - 1
                                || board[yCoord + i][xCoord + j]) {
                            return true;
                        }
                    }
                }
            }
            return false;
        }

        private function landingHandler(event:TetrisEvent):void {
            stick(event.tetromino);
            destroyFullLines();
        }

        private function stick(t:Tetromino):void {
            for (var i:int = 0; i < t.size; i++) {
                for (var j:int = 0; j < t.size; j++) {
                    if (t.shape[i][j]) {
                        board[t.yCoord + i][t.xCoord + j] = t.color;
                    }
                }
            }
            drawBlocks();
        }

        private function destroyFullLines():void {
            var nonFullLines:Array = board.filter(
                    function (line:Array, i:int, array:Array):Boolean {
                        return !isLineFull(line);
                    });
            var destroyedLinesCount:int = HEIGHT - nonFullLines.length;
            if (destroyedLinesCount > 0) {
                var emptyLines:Array =
                    Utils.createArray2D(WIDTH, destroyedLinesCount);
                board = emptyLines.concat(nonFullLines);
                draw();

                dispatchEvent(new TetrisEvent(TetrisEvent.LINES_DESTROYED,
                            null, destroyedLinesCount));
            }
        }

        private function isLineFull(line:Array):Boolean {
            return line.every(
                    function (block:Object, i:int, array:Array):Boolean {
                        return block != null;
                    });
        }

        private function draw():void {
            drawBounds();
            drawBlocks();
        }

        private function drawBounds():void {
            graphics.clear();
            graphics.lineStyle(1.0, this.lineColor);
            graphics.beginFill(this.fillColor);
            graphics.drawRect(
                    bounds.left, bounds.top,
                    bounds.width, bounds.height);
            graphics.endFill();
        }

        private function drawBlocks():void {
            for (var i:int = 0; i < HEIGHT; i++) {
                for (var j:int = 0; j < WIDTH; j++) {
                    if (board[i][j]) {
                        graphics.beginFill(board[i][j]);
                        graphics.drawRect(
                                j * Board.BLOCK_SIZE, i * Board.BLOCK_SIZE,
                                Board.BLOCK_SIZE, Board.BLOCK_SIZE);
                        graphics.endFill();
                    }
                }
            }
        }
    }
}
