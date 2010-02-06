package tetris {

    import flash.display.Shape;
    import flash.utils.ByteArray;

    import mx.utils.ObjectUtil;

    public class Tetromino extends Shape {

        public var shape:Array;
        public var size:int;
        public var color:uint;
        public var xCoord:int;
        public var yCoord:int;

        public function Tetromino(shape:Array, size:int, color:uint):void {
            this.shape = shape;
            this.size = size;
            this.color = color;
            xCoord = (Board.WIDTH / 2) - (size / 2);
            yCoord = 0;
            updatePosition();
            draw();
        }

        public function moveDown():void {
            var nextPosition:Tetromino = shallowCopy(this);
            nextPosition.yCoord += 1;
            attemptMove(nextPosition);
        }

        public function moveLeft():void {
            var nextPosition:Tetromino = shallowCopy(this);
            nextPosition.xCoord -= 1;
            attemptMove(nextPosition);
        }

        public function moveRight():void {
            var nextPosition:Tetromino = shallowCopy(this);
            nextPosition.xCoord += 1;
            attemptMove(nextPosition);
        }

        public function rotateClockwise():void {
            var nextPosition:Tetromino = shallowCopy(this);
            var rotatedShape:Array = Utils.createArray2D(size, size);
            for (var i:int = 0; i < size; i++) {
                for (var j:int = 0; j < size; j++) {
                    rotatedShape[(size - 1) - j][i] = shape[i][j];
                }
            }
            nextPosition.shape = rotatedShape;
            attemptMove(nextPosition);
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

        private function updatePosition():void {
            x = xCoord * Board.BLOCK_SIZE;
            y = yCoord * Board.BLOCK_SIZE + 2;
        }

        private function attemptMove(nextPosition:Tetromino):void {
            if (!(parent as Board).isConflictWith(nextPosition)) {
                copyDataFrom(nextPosition);
                updatePosition();
            }
        }

        private function shallowCopy(original:Tetromino):Tetromino {
            var copy:Tetromino =
                new Tetromino(original.shape, original.size, original.color);
            copy.xCoord = original.xCoord;
            copy.yCoord = original.yCoord;
            copy.updatePosition();
            return copy;
        }

        private function copyDataFrom(source:Tetromino):void {
            shape = source.shape;
            size = source.size;
            color = source.color;
            xCoord = source.xCoord;
            yCoord = source.yCoord;
        }
    }
}
