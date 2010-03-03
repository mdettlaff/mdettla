package tt {

    import flash.events.KeyboardEvent;
    import flash.system.Capabilities;
    import flash.utils.ByteArray;

    public class PolishChars {

        private var char:ByteArray = new ByteArray();
        private var isPrevUTF8Marker:Boolean = false;

        public function charFrom(event:KeyboardEvent):String {
            if (Capabilities.os.indexOf("Linux") != -1) {
                return charForLinux(event);
            } else {
                return charForWindows(event);
            }
        }

        private function charForLinux(event:KeyboardEvent):String {
            const ISO_8859_2_PRINTABLE:String =
                " Ą˘Ł¤ĽŚ§¨ŠŞŤŹ­ŽŻ°ą˛ł´ľśˇ¸šşťź˝žżŔÁÂĂÄĹĆÇČÉĘËĚÍÎĎ" +
                "ĐŃŇÓÔŐÖ×ŘŮÚŰÜÝŢßŕáâăäĺćçčéęëěíîďđńňóôőö÷řůúűüýţ˙";
            char.writeByte(event.charCode);
            var isUTF8Marker:Boolean =
                event.charCode > 194 && event.charCode < 198;
            if (isUTF8Marker) {
                isPrevUTF8Marker = true;
                return null;
            } else {
                var c:String;
                char.position = 0;
                if (isPrevUTF8Marker) {
                    c = char.readUTFBytes(2);
                } else if (event.charCode > 160 && event.charCode < 256) {
                    c = ISO_8859_2_PRINTABLE.charAt(event.charCode - 160);
                } else {
                    c = String.fromCharCode(event.charCode);
                }
                char.position = 0;
                isPrevUTF8Marker = false;
                return c;
            }
        }

        private function charForWindows(event:KeyboardEvent):String {
            const EN_TO_PL:Object = {
                'a': 'ą', 'c': 'ć', 'e': 'ę', 'l': 'ł', 'n': 'ń', 'o': 'ó',
                's': 'ś', 'z': 'ż', 'x': 'ź', 'A': 'Ą', 'C': 'Ć', 'E': 'Ę',
                'L': 'Ł', 'N': 'Ń', 'O': 'Ó', 'S': 'Ś', 'Z': 'Ż', 'X': 'Ź'
            }
            const c:String = String.fromCharCode(event.charCode);
            return event.altKey && c in EN_TO_PL ? EN_TO_PL[c] : c;
        }
    }
}
