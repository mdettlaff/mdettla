package tetris {

    import flash.display.DisplayObjectContainer;

    public class TetrominoCreator {

        public static const I:uint = 0;
        public static const J:uint = 1;
        public static const L:uint = 2;
        public static const O:uint = 3;
        public static const S:uint = 4;
        public static const T:uint = 5;
        public static const Z:uint = 6;

        private static const CYAN:uint = 0x00FFFF;
        private static const BLUE:uint = 0x0000FF;
        private static const ORANGE:uint = 0xFF7F00;
        private static const YELLOW:uint = 0xFFFF00;
        private static const GREEN:uint = 0x00FF00;
        private static const PURPLE:uint = 0xA000A0;
        private static const RED:uint = 0xFF0000;

        public function addTetromino(shape:uint,
                target:DisplayObjectContainer):Tetromino {
            var tetromino:Tetromino = this.createTetromino(shape);
            tetromino.draw();
            target.addChild(tetromino);
            return tetromino;
        }

        private function createTetromino(shape:uint):Tetromino {
            switch (shape) {
                case I:
                    trace("creating I tetromino");
                    return new Tetromino([
                            [1, 0, 0, 0],
                            [1, 0, 0, 0],
                            [1, 0, 0, 0],
                            [1, 0, 0, 0]], 4, CYAN);
                    break;
                case J:
                    trace("creating J tetromino");
                    return new Tetromino([
                            [0, 1, 0],
                            [0, 1, 0],
                            [1, 1, 0]], 3, BLUE);
                    break;
                case L:
                    trace("creating L tetromino");
                    return new Tetromino([
                            [1, 0, 0],
                            [1, 0, 0],
                            [1, 1, 0]], 3, ORANGE);
                    break;
            }
            throw new Error("unknown tetromino shape: " + shape);
        }
    }
}
