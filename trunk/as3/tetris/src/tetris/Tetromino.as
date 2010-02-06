package tetris {

    import flash.display.Shape;

    public class Tetromino extends Shape {

        public var shape:Array;
        public var size:int;
        public var color:uint;
        public var xCoord:int;
        public var yCoord:int;

        /*
         * Potential next tetromino state. Possible changes of state are
         * rotations and moving one block down, left or right.
         */
        private var nextState:Tetromino;

        public function Tetromino(shape:Array, size:int, color:uint):void {
            this.shape = shape;
            this.size = size;
            this.color = color;
            xCoord = (Board.WIDTH / 2) - (size / 2);
            yCoord = 0;
            updateXY();
            draw();
        }

        public function moveDown():void {
            attemptMove(shape, xCoord, yCoord + 1);
        }

        public function moveLeft():void {
            attemptMove(shape, xCoord - 1, yCoord);
        }

        public function moveRight():void {
            attemptMove(shape, xCoord + 1, yCoord);
        }

        public function rotateClockwise():void {
            var rotatedShape:Array = Utils.createArray2D(size, size);
            for (var i:int = 0; i < size; i++) {
                for (var j:int = 0; j < size; j++) {
                    rotatedShape[(size - 1) - j][i] = shape[i][j];
                }
            }
            attemptMove(rotatedShape, xCoord, yCoord);
            draw();
        }

        private function draw():void {
            graphics.clear();
            graphics.beginFill(color);
            for (var i:int = 0; i < size; i++) {
                for (var j:int = 0; j < size; j++) {
                    if (shape[i][j]) {
                        graphics.drawRect(
                                i * Board.BLOCK_SIZE, j * Board.BLOCK_SIZE,
                                Board.BLOCK_SIZE, Board.BLOCK_SIZE);
                    }
                }
            }
            graphics.endFill();
        }

        private function updateXY():void {
            x = xCoord * Board.BLOCK_SIZE;
            y = yCoord * Board.BLOCK_SIZE + 2;
        }

        private function move(targetState:Tetromino):void {
            shape = targetState.shape;
            xCoord = targetState.xCoord;
            yCoord = targetState.yCoord;
            updateXY();
        }

        private function attemptMove(shape:Array,
                xCoord:int, yCoord:int):void {
            if (nextState == null) {
                nextState = new Tetromino(shape, size, color);
            }
            nextState.shape = shape;
            nextState.xCoord = xCoord;
            nextState.yCoord = yCoord;
            if (!(parent as Board).isConflictWith(nextState)) {
                move(nextState);
            }
        }
    }
}
