package {

    import com.hurlant.crypto.hash.HMAC;
    import com.hurlant.crypto.hash.IHash;
    import com.hurlant.crypto.hash.IHMAC;
    import com.hurlant.crypto.hash.SHA1;
    import com.hurlant.util.Hex;

    import flash.display.Sprite;
    import flash.utils.ByteArray;

    public class Test extends Sprite {

        public function Test() {
            var byteArray:ByteArray = new ByteArray();
            byteArray.writeByte(160);
            byteArray.writeByte(32);
            for (var i:int = 0; i < byteArray.length; i++) {
                trace("byteArray[" + i + "] = " + byteArray[i]);
            }
            trace("Hex.fromArray(byteArray) = " + Hex.fromArray(byteArray));

            var data:String = "The quick brown fox jumped over the lazy dog.";
            var key:String = "secret";
            var hmac:IHMAC = new HMAC(new SHA1());
            var code:String = Hex.fromArray(hmac.compute(
                        Hex.toArray(Hex.fromString(key)),
                        Hex.toArray(Hex.fromString(data))));
            trace("HMAC(" + key + ", " + data + ") = " + code);

            var sha1:IHash = new SHA1();
            trace("SHA1(" + data + ") = "
                    + Hex.fromArray(sha1.hash(
                            Hex.toArray(Hex.fromString(data)))));
        }
    }
}
