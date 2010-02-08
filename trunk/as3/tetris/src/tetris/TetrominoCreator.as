package tetris {

    public class TetrominoCreator {

        // tetromino shapes
        private static const I:String = "I";
        private static const J:String = "J";
        private static const L:String = "L";
        private static const O:String = "O";
        private static const S:String = "S";
        private static const T:String = "T";
        private static const Z:String = "Z";

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
            trace("bagOfShapes =", bagOfShapes);
            var shape:String = bagOfShapes.pop();
            if (bagOfShapes.length == 0) {
                bagOfShapes = createBagOfShapes();
            }
            return createTetromino(shape);
        }

        private function createBagOfShapes():Array {
            return Utils.shuffle([I, J, L, O, S, T, Z]);
        }

        private function createTetromino(shape:String):Tetromino {
            switch (shape) {
                case I:
                    return new Tetromino([
                            [0, 0, 0, 0],
                            [1, 1, 1, 1],
                            [0, 0, 0, 0],
                            [0, 0, 0, 0]], CYAN);
                    break;
                case J:
                    return new Tetromino([
                            [1, 0, 0],
                            [1, 1, 1],
                            [0, 0, 0]], BLUE);
                    break;
                case L:
                    return new Tetromino([
                            [0, 0, 1],
                            [1, 1, 1],
                            [0, 0, 0]], ORANGE);
                    break;
                case O:
                    return new Tetromino([
                            [1, 1],
                            [1, 1]], YELLOW);
                    break;
                case S:
                    return new Tetromino([
                            [0, 1, 1],
                            [1, 1, 0],
                            [0, 0, 0]], GREEN);
                    break;
                case T:
                    return new Tetromino([
                            [0, 1, 0],
                            [1, 1, 1],
                            [0, 0, 0]], PURPLE);
                    break;
                case Z:
                    return new Tetromino([
                            [1, 1, 0],
                            [0, 1, 1],
                            [0, 0, 0]], RED);
                    break;
            }
            throw new Error("unknown tetromino shape:", shape);
        }
    }
}
