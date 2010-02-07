package tetris {

    public class TetrominoCreator {

        public static const SHAPES_COUNT:int = 7;
        // tetromino shapes
        public static const I:int = 0;
        public static const J:int = 1;
        public static const L:int = 2;
        public static const O:int = 3;
        public static const S:int = 4;
        public static const T:int = 5;
        public static const Z:int = 6;

        private static const CYAN:uint = 0x00FFFF;
        private static const BLUE:uint = 0x0000FF;
        private static const ORANGE:uint = 0xFF7F00;
        private static const YELLOW:uint = 0xFFFF00;
        private static const GREEN:uint = 0x00FF00;
        private static const PURPLE:uint = 0xA000A0;
        private static const RED:uint = 0xFF0000;

        private var bagOfShapes:Array;

        public function TetrominoCreator() {
            bagOfShapes = createBagOfShapes();
        }

        public function getNextTetromino():Tetromino {
            trace("bagOfShapes = " + bagOfShapes);
            var shape:int = bagOfShapes.pop();
            if (bagOfShapes.length == 0) {
                bagOfShapes = createBagOfShapes();
            }
            return createTetromino(shape);
        }

        private function createBagOfShapes():Array {
            return Utils.shuffle(Utils.range(SHAPES_COUNT));
        }

        private function createTetromino(shape:int):Tetromino {
            switch (shape) {
                case I:
                    return new Tetromino([
                            [0, 0, 0, 0],
                            [1, 1, 1, 1],
                            [0, 0, 0, 0],
                            [0, 0, 0, 0]], 4, CYAN);
                    break;
                case J:
                    return new Tetromino([
                            [1, 0, 0],
                            [1, 1, 1],
                            [0, 0, 0]], 3, BLUE);
                    break;
                case L:
                    return new Tetromino([
                            [0, 0, 1],
                            [1, 1, 1],
                            [0, 0, 0]], 3, ORANGE);
                    break;
                case O:
                    return new Tetromino([
                            [1, 1],
                            [1, 1]], 2, YELLOW);
                    break;
                case S:
                    return new Tetromino([
                            [0, 1, 1],
                            [1, 1, 0],
                            [0, 0, 0]], 3, GREEN);
                    break;
                case T:
                    return new Tetromino([
                            [0, 1, 0],
                            [1, 1, 1],
                            [0, 0, 0]], 3, PURPLE);
                    break;
                case Z:
                    return new Tetromino([
                            [1, 1, 0],
                            [0, 1, 1],
                            [0, 0, 0]], 3, RED);
                    break;
            }
            throw new Error("unknown tetromino shape: " + shape);
        }
    }
}
