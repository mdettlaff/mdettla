<?php

function rand_str($length) {
    $chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    $str = '';
    $count = strlen($chars);
    while ($length--) {
        $str .= $chars[mt_rand(0, $count - 1)];
    }
    return $str;
}

function hmac($data, $key) {
    $hash_function = sha1;
    if (!function_exists($hash_function)) {
        return null;
    }
    $blocksize = 64;
    if (strlen($key) > $blocksize) {
        $key = $hash_function($key);
    }
    $key = str_pad($key, $blocksize, chr(0x00));
    $opad = str_repeat(chr(0x5c), $blocksize) ^ $key;
    $ipad = str_repeat(chr(0x36), $blocksize) ^ $key;
    return $hash_function($opad . pack('H*', $hash_function($ipad . $data)));
}

function is_hmac_valid($hash, $h_data, $h_key) {
    if (empty($hash) || empty($h_data)) {
        return false;
    }
    $valid_hash = hmac($h_data, $h_key);
    return $hash == $valid_hash;
}

function verify_typing_time($minutes, $seconds, $time_verifier) {
    return abs($time_verifier / ($minutes * 60 + $seconds) - 1) < 0.1;
}

function latin2_to_utf8($latin2_str) {
    $UNKNOWN_CHAR = ' ';
    $utf8_str = '';
    for ($i = 0; $i < strlen($latin2_str); $i++) {
        $c = substr($latin2_str, $i, 1);
        $charcode = ord($c);
        if ($charcode < 127) {
            $utf8_str .= $c;
        } else {
            switch ($charcode) {
                case 0xb1: $utf8_str .= chr(0xc4) . chr(0x85); break;
                case 0xe6: $utf8_str .= chr(0xc4) . chr(0x87); break;
                case 0xea: $utf8_str .= chr(0xc4) . chr(0x99); break;
                case 0xb3: $utf8_str .= chr(0xc5) . chr(0x82); break;
                case 0xf1: $utf8_str .= chr(0xc5) . chr(0x84); break;
                case 0xf3: $utf8_str .= chr(0xc3) . chr(0xb3); break;
                case 0xb6: $utf8_str .= chr(0xc5) . chr(0x9b); break;
                case 0xbf: $utf8_str .= chr(0xc5) . chr(0xbc); break;
                case 0xbc: $utf8_str .= chr(0xc5) . chr(0xba); break;
                case 0xa1: $utf8_str .= chr(0xc4) . chr(0x84); break;
                case 0xc6: $utf8_str .= chr(0xc4) . chr(0x86); break;
                case 0xca: $utf8_str .= chr(0xc4) . chr(0x98); break;
                case 0xa3: $utf8_str .= chr(0xc5) . chr(0x81); break;
                case 0xd1: $utf8_str .= chr(0xc5) . chr(0x83); break;
                case 0xd3: $utf8_str .= chr(0xc3) . chr(0x93); break;
                case 0xa6: $utf8_str .= chr(0xc5) . chr(0x9a); break;
                case 0xaf: $utf8_str .= chr(0xc5) . chr(0xbb); break;
                case 0xac: $utf8_str .= chr(0xc5) . chr(0xb9); break;
                default:
                    $utf8_str .= $UNKNOWN_CHAR;
                    break;
            }
        }
    }
    return $utf8_str;
}

function utf8_to_latin2($utf8_str) {
    $UNKNOWN_CHAR = ' ';
    $latin2_str = '';
    for ($i = 0; $i < strlen($utf8_str); $i++) {
        $c = substr($utf8_str, $i, 1);
        $charcode = ord($c);
        if ($charcode < 127) {
            $latin2_str .= $c;
        } else if ($charcode == 0xc3) {
            $charcode = ord(substr($utf8_str, ++$i, 1));
            switch ($charcode) {
                case 0x93: $latin2_str .= chr(0xd3); break;
                case 0xb3: $latin2_str .= chr(0xf3); break;
                default: $latin2_str .= $UNKNOWN_CHAR; break;
            }
        } else if ($charcode == 0xc4) {
            $charcode = ord(substr($utf8_str, ++$i, 1));
            switch ($charcode) {
                case 0x85: $latin2_str .= chr(0xb1); break;
                case 0x87: $latin2_str .= chr(0xe6); break;
                case 0x99: $latin2_str .= chr(0xea); break;
                case 0x84: $latin2_str .= chr(0xa1); break;
                case 0x86: $latin2_str .= chr(0xc6); break;
                case 0x98: $latin2_str .= chr(0xca); break;
                default: $latin2_str .= $UNKNOWN_CHAR; break;
            }
        } else if ($charcode == 0xc5) {
            $charcode = ord(substr($utf8_str, ++$i, 1));
            switch ($charcode) {
                case 0x82: $latin2_str .= chr(0xb3); break;
                case 0x84: $latin2_str .= chr(0xf1); break;
                case 0x9b: $latin2_str .= chr(0xb6); break;
                case 0xbc: $latin2_str .= chr(0xbf); break;
                case 0xba: $latin2_str .= chr(0xbc); break;
                case 0x81: $latin2_str .= chr(0xa3); break;
                case 0x83: $latin2_str .= chr(0xd1); break;
                case 0x9a: $latin2_str .= chr(0xa6); break;
                case 0xbb: $latin2_str .= chr(0xaf); break;
                case 0xb9: $latin2_str .= chr(0xac); break;
                default: $latin2_str .= $UNKNOWN_CHAR; break;
            }
        } else {
            $latin2_str .= $UNKNOWN_CHAR;
        }
    }
    return $latin2_str;
}

?>
