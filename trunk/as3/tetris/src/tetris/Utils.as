package tetris {

    public class Utils {

        public static function createArray2D(size:int):Array {
            var array:Array = new Array(size);
            for (var i:int = 0; i < size; i++) {
                array[i] = new Array(size);
            }
            return array;
        }
    }
}
