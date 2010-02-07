package tetris {

    public class Utils {

        public static function createArray2D(sizeX:int, sizeY:int):Array {
            var array2D:Array = new Array(sizeY);
            for (var i:int = 0; i < sizeY; i++) {
                array2D[i] = new Array(sizeX);
            }
            return array2D;
        }

        public static function randInt(n:int):int {
            return Math.round((n - 1) * Math.random());
        }

        public static function range(n:int):Array {
            var range:Array = new Array(n);
            for (var i:int = 0; i < n; i++) {
                range[i] = i;
            }
            return range;
        }

        public static function shuffle(array:Array):Array {
            var original:Array = array.slice(0, array.length);
            var shuffled:Array = [];
            while (original.length > 0) {
                shuffled.push(original.splice(randInt(original.length), 1));
            }
            return shuffled;
        }
    }
}
