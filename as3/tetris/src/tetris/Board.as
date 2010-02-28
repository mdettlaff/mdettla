package tetris {

    import flash.display.Sprite;
    import flash.geom.Rectangle;
    import flash.text.TextField;
    import flash.text.TextFieldAutoSize;
    import flash.text.TextFormat;

    public class Board extends Sprite {

        public static const WIDTH:int = 10;
        public static const HEIGHT:int = 20;
        public static const BLOCK_SIZE:int = 15;

        private var board:Array;

        private var bounds:Rectangle;
        private var lineColor:uint;
        private var fillColor:uint;

        public function Board(fillColor:uint = 0xFFFFFF,
                lineColor:uint = 0x000000) {
            this.fillColor = fillColor;
            this.lineColor = lineColor;
            clear();
            bounds =
                new Rectangle(0, 0, WIDTH * BLOCK_SIZE, HEIGHT * BLOCK_SIZE);
            drawBounds();

            addEventListener(TetrisEvent.TETROMINO_LANDED, onLanding);
        }

        public function isConflictWithTetrominoState(
                shape:Array, xCoord:int, yCoord:int):Boolean {
            for (var i:int = 0; i < shape.length; i++) {
                for (var j:int = 0; j < shape.length; j++) {
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

        public function reset():void {
            Utils.removeAllChildren(this);
            clear();
            draw();
        }

        public function gameOver():void {

            function createGameOverLabel():TextField {
                var format:TextFormat = new TextFormat();
                format.font = "Verdana";
                format.size = 18;

                var label:TextField = new TextField();
                label.defaultTextFormat = format;
                label.text = "Koniec gry";
                label.autoSize = TextFieldAutoSize.CENTER;
                label.x = (width / 2) - (label.width / 2);
                label.y = (height / 2) - 50;
                label.background = true;
                label.border = true;
                label.selectable = false;
                return label;
            };

            addChild(createGameOverLabel());
            draw();
        }

        private function onLanding(event:TetrisEvent):void {

            function stick(t:Tetromino):void {
                for (var i:int = 0; i < t.shape.length; i++) {
                    for (var j:int = 0; j < t.shape.length; j++) {
                        if (t.shape[i][j]) {
                            board[t.yCoord + i][t.xCoord + j] = t.color;
                        }
                    }
                }
                drawBlocks();
            };

            stick(event.tetromino);
            removeChild(event.tetromino);
            destroyFullLines();
        }

        private function clear():void {
            board = Utils.createArray2D(WIDTH, HEIGHT);
        }

        private function destroyFullLines():void {

            function isLineNonFull(line:Array, i:int, array:Array):Boolean {
                return line.some(
                        function (block:Object, i:int, array:Array):Boolean {
                            return block == null;
                        });
            };

            var nonFullLines:Array = board.filter(isLineNonFull);
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
