package tetris {

    import tetris.Board;
    import tetris.Tetromino;
    import tetris.TetrominoCreator;

    import flash.utils.Timer;
    import flash.events.Event;
    import flash.events.TimerEvent;
    import flash.events.KeyboardEvent;
    import flash.display.DisplayObjectContainer;
    import flash.display.Sprite;
    import flash.ui.Keyboard;

    public class Tetris extends Sprite {

        private static const SPEED:int = 1000;

        private var board:Board;
        private var tetrominoCreator:TetrominoCreator;
        private var tetromino:Tetromino;

        public function Tetris(mainContainer:DisplayObjectContainer) {
            board = new Board();
            tetrominoCreator = new TetrominoCreator();
            putNextTetrominoOnBoard();

            board.addChild(tetromino);
            mainContainer.addChild(board);

            var timer:Timer = new Timer(SPEED, 0);
            timer.addEventListener(TimerEvent.TIMER, timerHandler);
            timer.start();

            mainContainer.addEventListener(KeyboardEvent.KEY_DOWN, keyHandler);
            board.addEventListener(
                    TetrisEvent.TETROMINO_STUCK, tetrominoStuckHandler);
        }

        private function timerHandler(event:TimerEvent):void {
            tetromino.moveDown();
        }

        private function keyHandler(event:KeyboardEvent):void {
            switch (event.keyCode) {
                case Keyboard.LEFT:
                    tetromino.moveLeft();
                    break;
                case Keyboard.RIGHT:
                    tetromino.moveRight();
                    break;
                case Keyboard.DOWN:
                    tetromino.moveDown();
                    break;
                case Keyboard.UP:
                    tetromino.rotateClockwise();
                    break;
            }
        }

        private function tetrominoStuckHandler(event:TetrisEvent):void {
            putNextTetrominoOnBoard();
        }

        private function putNextTetrominoOnBoard():void {
            tetromino = tetrominoCreator.getNextTetromino();
            board.addChild(tetromino);
        }
    }
}
