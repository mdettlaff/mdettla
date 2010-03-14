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

?>
