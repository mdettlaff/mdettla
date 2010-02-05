package tetris {

    import flash.display.Shape;

    public class Tetromino extends Shape {

        private var shape:Array;
        private var size:int;
        private var color:uint;

        public function Tetromino(shape:Array, size:int, color:uint):void {
            this.shape = shape;
            this.size = size;
            this.color = color;
            this.x = (Board.WIDTH / 2) * Board.BLOCK_SIZE;
            this.y = Board.BLOCK_SIZE;
        }

        public function draw():void {
            graphics.clear();
            graphics.beginFill(color);
            for (var i:int = 0; i < size; i++) {
                for (var j:int = 0; j < size; j++) {
                    if (this.shape[i][j]) {
                        graphics.drawRect(
                                i * Board.BLOCK_SIZE, j * Board.BLOCK_SIZE,
                                Board.BLOCK_SIZE, Board.BLOCK_SIZE);
                    }
                }
            }
            graphics.endFill();
        }

        public function moveDown():void {
            this.y += Board.BLOCK_SIZE;
        }

        public function moveLeft():void {
            this.x -= Board.BLOCK_SIZE;
        }

        public function moveRight():void {
            this.x += Board.BLOCK_SIZE;
        }

        public function rotateClockwise():void {
            var newShape:Array = Utils.createArray2D(size);
            for (var i:int = 0; i < size; i++) {
                for (var j:int = 0; j < size; j++) {
                    newShape[(size - 1) - j][i] = shape[i][j];
                }
            }
            shape = newShape;
            draw();
        }
    }
}
