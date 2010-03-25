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

function is_hmac_valid($hash, $h_data, $h_key) {
    if (empty($hash) || empty($h_data)) {
        return false;
    }
    $valid_hash = hash_hmac('sha1', $h_data, $h_key);
    return $hash == $valid_hash;
}

function verify_typing_time($minutes, $seconds, $time_verifier) {
    return abs($time_verifier / ($minutes * 60 + $seconds) - 1) < 0.1;
}

?>
