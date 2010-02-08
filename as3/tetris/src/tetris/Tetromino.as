package tetris {

    import flash.display.Shape;
    import flash.events.KeyboardEvent;
    import flash.ui.Keyboard;

    public class Tetromino extends Shape {

        public var shape:Array;
        public var size:int;
        public var color:uint;
        public var xCoord:int;
        public var yCoord:int;

        public function Tetromino(shape:Array, size:int, color:uint) {
            addEventListener(KeyboardEvent.KEY_DOWN, keyHandler);
            this.shape = shape;
            this.size = size;
            this.color = color;
            xCoord = (Board.WIDTH / 2) - (size / 2);
            yCoord = 0;
            updateXY();
            draw();
        }

        public function moveDown():void {
            if (!attemptMove(shape, xCoord, yCoord + 1)) {
                (parent as Board).stick(this);
                (parent.parent as Tetris).putNextTetrominoOnBoard();
            }
        }

        private function keyHandler(event:KeyboardEvent):void {
            switch (event.keyCode) {
                case Keyboard.LEFT:
                    moveLeft();
                    break;
                case Keyboard.RIGHT:
                    moveRight();
                    break;
                case Keyboard.DOWN:
                    moveDown();
                    break;
                case Keyboard.UP:
                    rotateClockwise();
                    break;
            }
        }

        private function moveLeft():void {
            attemptMove(shape, xCoord - 1, yCoord);
        }

        private function moveRight():void {
            attemptMove(shape, xCoord + 1, yCoord);
        }

        private function rotateClockwise():void {
            var rotatedShape:Array = Utils.createArray2D(size, size);
            for (var i:int = 0; i < size; i++) {
                for (var j:int = 0; j < size; j++) {
                    rotatedShape[i][(size - 1) - j] = shape[j][i];
                }
            }
            attemptMove(rotatedShape, xCoord, yCoord);
            draw();
        }

        private function draw():void {
            graphics.clear();
            graphics.lineStyle(1.0, 0x000000, 1.0);
            graphics.beginFill(color);
            for (var i:int = 0; i < size; i++) {
                for (var j:int = 0; j < size; j++) {
                    if (shape[i][j]) {
                        graphics.drawRect(
                                j * Board.BLOCK_SIZE, i * Board.BLOCK_SIZE,
                                Board.BLOCK_SIZE, Board.BLOCK_SIZE);
                    }
                }
            }
            graphics.endFill();
        }

        private function attemptMove(
                shape:Array, xCoord:int, yCoord:int):Boolean {
            if (!(parent as Board).isConflictWithTetrominoState(
                        shape, size, xCoord, yCoord)) {
                moveToState(shape, xCoord, yCoord);
                return true;
            }
            return false;
        }

        private function moveToState(
                shape:Array, xCoord:int, yCoord:int):void {
            this.xCoord = xCoord;
            this.yCoord = yCoord;
            this.shape = shape;
            updateXY();
        }

        private function updateXY():void {
            x = xCoord * Board.BLOCK_SIZE;
            y = yCoord * Board.BLOCK_SIZE;
        }
    }
}
