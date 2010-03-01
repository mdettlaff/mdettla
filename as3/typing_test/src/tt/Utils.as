package tt {

    import mx.utils.StringUtil;

    public class Utils {

        private static const PL_TO_EN:Object = {
            'ą': 'a', 'ć': 'c', 'ę': 'e', 'ł': 'l', 'ń': 'n', 'ó': 'o',
            'ś': 's', 'ż': 'z', 'ź': 'z', 'Ą': 'A', 'Ć': 'C', 'Ę': 'E',
            'Ł': 'L', 'Ń': 'N', 'Ó': 'O', 'Ś': 'S', 'Ż': 'Z', 'Ź': 'Z'
        }

        public static function breakLines(
                text:String, maxLineLength:int):Array /* of String */ {
            const multiSpace:RegExp = / +/g;
            var textLines:Array =
                StringUtil.trim(text.replace(multiSpace, ' ')).split('\n');
            var lineEndIndex:int = 0;
            for (var i:int = 0; i < textLines.length; i++) {
                for (var j:int = 1; j < textLines[i].length
                        && j <= maxLineLength; j++) {
                    if (textLines[i].charAt(j) == ' '
                            || textLines[i].charAt(j) == '\t') {
                        lineEndIndex = j;
                    }
                    if (j == maxLineLength) { // break line
                        textLines.splice(i + 1, 0,
                                textLines[i].substring(lineEndIndex + 1));
                        textLines[i] = textLines[i].substring(0, lineEndIndex);
                    }
                }
            }
            return textLines;
        }

        public static function shavePlChars(withPlChars:String):String {
            var withoutPlChars:String = "";
            for (var i:int = 0; i < withPlChars.length; i++) {
                var c:String = withPlChars.charAt(i);
                withoutPlChars += c in PL_TO_EN ? PL_TO_EN[c] : c;
            }
            return withoutPlChars;
        }

        public static function containsPlChars(s:String,
                index:int = 0, array:Array = null):Boolean {
            for (var i:int = 0; i < s.length; i++) {
                if (s.charAt(i) in PL_TO_EN) {
                    return true;
                }
            }
            return false;
        }
    }
}
