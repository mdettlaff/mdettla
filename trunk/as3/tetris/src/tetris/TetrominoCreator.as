package tetris {

    import flash.display.DisplayObjectContainer;

    public class TetrominoCreator {
        public static const L:uint = 0;
        public static const I:uint = 1;

        public function addTetromino(shape:uint,
                target:DisplayObjectContainer):Tetromino {
            var tetromino:Tetromino = this.createTetromino(shape);
            tetromino.draw();
            target.addChild(tetromino);
            return tetromino;
        }

        private function createTetromino(shape:uint):Tetromino {
            switch (shape) {
                case L:
                    trace("creating new tetromino");
                    return new Tetromino();
                    break;
            }
            throw new Error("unknown tetromino shape: " + shape);
        }
    }
}
