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
                } else {
                    if (event.charCode > 160 && event.charCode < 256) {
                        c = ISO_8859_2_PRINTABLE.charAt(event.charCode - 160);
                    } else {
                        c = String.fromCharCode(event.charCode);
                    }
                }
                char.position = 0;
                isPrevUTF8Marker = false;
                return c;
            }
        }

        private function charForWindows(event:KeyboardEvent):String {
            const EN:String = "aAcCeElLnNoOsSzZxX";
            const PL:String = "ąĄćĆęĘłŁńŃóÓśŚżŻźŹ";
            var index:int;
            if (event.altKey) {
                index = EN.indexOf(String.fromCharCode(event.charCode));
                if (index != -1) {
                    return String.fromCharCode(PL.charCodeAt(index));
                }
            }
            return String.fromCharCode(event.charCode);
        }
    }
}
