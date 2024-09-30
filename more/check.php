<?php
function checker($userId, $id, $key, $mode) {
    if ($mode === 'whitelist') {
        $url = "https://api-gateway.platoboost.com/v1/public/whitelist/$userId/$id?key=idklol";
    } elseif ($mode === 'check') {
        $url = "https://api-gateway.platoboost.com/v1/authenticators/redeem/$userId/$id/$key";
    } else {
        return false;
    }

    $ch = curl_init();
    if ($mode === 'whitelist') {
        curl_setopt($ch, CURLOPT_URL, $url);
        curl_setopt($ch, CURLOPT_HTTPGET, true);
    } else {
        curl_setopt($ch, CURLOPT_URL, $url);
        curl_setopt($ch, CURLOPT_POST, true);
    }

    curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, false);
    $response = curl_exec($ch);
    $error = curl_error($ch);
    curl_close($ch);

    if ($error) {
        return false;
    }

    $result = json_decode($response, true);
    if (is_array($result) && isset($result['success'])) {
        return $result['success'];
    } else {
        return false;
    }
}


function base64UrlEncode($data) {
            return str_replace(['+', '/', '='], ['-', '_', ''], base64_encode($data));
        }
function base64UrlDecode($input) {
    return base64_decode(strtr($input, '-_', '+/'));
}



function createToken($key, $claims) {
    $header = ['typ' => 'JWT', 'alg' => 'HS256'];
    $payload = $claims;
    
    $encodedHeader = base64UrlEncode(json_encode($header));
    $encodedPayload = base64UrlEncode(json_encode($payload));
    
    $signature = hash_hmac('sha256', "$encodedHeader.$encodedPayload", $key, false);
    $encodedSignature = base64UrlEncode($signature);

    return "$encodedHeader.$encodedPayload.$encodedSignature";
}

function verifyJwt($token, $secret, $algorithm = 'HS256') {
    $parts = explode('.', $token);
    if (count($parts) != 3) {
        return false;
    }

    list($headerBase64, $payloadBase64, $signatureBase64) = $parts;

    $decodedPayload = json_decode(base64UrlDecode($payloadBase64), true);
    if (!is_array($decodedPayload)) {
        return false;
    }

    $decodedHeader = json_decode(base64UrlDecode($headerBase64), true);
    if (!is_array($decodedHeader) || !isset($decodedHeader['alg']) || $decodedHeader['alg'] !== $algorithm) {
        return false;
    }

    $expectedSignature = hash_hmac('sha256', "$headerBase64.$payloadBase64", $secret, false);
    if (!($expectedSignature==base64UrlDecode($signatureBase64))) {
   
        return false;
    }

    if ( isset($decodedPayload['exp']) && isset($decodedPayload['iat']) && is_numeric($decodedPayload['exp'])) {
        $expiryTime = (int)$decodedPayload['exp'];
        if (time() >= $expiryTime && $expiryTime >$decodedPayload['iat']) {
            return false;
        }
    }
   
        


    return $decodedPayload;
}
$yourkey = 'your_secret_key';//here is your key,don't leak it,or others can use the key to create a fake JWT



$jwt = $_GET['token'];
if (!isset($jwt)){
echo "error";
exit();
}
$verified = verifyJwt($jwt, $yourkey);
if ($verified !== false) {
$checkeridk=checker($verified["platoboost-id"],$verified["hwid"],$verified["key"] ?? "idkkey",$verified["mode"]);
  if ($checkeridk) {
  $yourdata=[
  "check"=> "true",
  "iat"=>time(),
  "exp"=>time()+25,// You can replace the 25
  ];
  echo createToken($yourkey,$yourdata);
  
  } else {
    $yourdata=[
  "check"=> "false",
  "iat"=>time(),
  "exp"=>time()+25,// You can replace the 25
  ];
  echo createToken($yourkey,$yourdata);
  }
} else {
    echo "LOL, are you stupid? Trying to crack it?\n";
}
