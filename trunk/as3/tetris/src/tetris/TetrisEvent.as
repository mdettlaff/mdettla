package tetris {

    import flash.events.Event;

    public class TetrisEvent extends Event {

        public static const TETROMINO_STUCK:String = "tetrominoStuck";
        public static const BLOCK_LINE_DESTROYED:String = "blockLineDestroyed";

        public var tetromino:Tetromino;

        public function TetrisEvent(
                eventName:String, tetromino:Tetromino=null) {
            super(eventName, true);
            this.tetromino = tetromino;
        }
    }
}
