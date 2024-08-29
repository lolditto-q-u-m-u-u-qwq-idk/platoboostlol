local platoboostid=123456 --num -account id
local platoboostcheckid=gethwid()--You don't have to use hwid, you can use any id, but the length is limited


function platoboostcheck_getkeylink()
  return string.format('https://gateway.platoboost.com/a/%d?id=%s',platoboostid,platoboostcheckid)
end--return your key link

function platoboostcheck_checkwhitelisted()--Check the whitelist. lol, I don't know what that key is for
  ditto=request({Url=string.format('https://api-gateway.platoboost.com/v1/public/whitelist/%d/%s?key=idklol',platoboostid,platoboostcheckid),
    Method="GET"
  })
  if ditto then
    if tostring(game:GetService("HttpService"):JSONDecode(ditto.Body).success) == "true" then
      return true
     else
      return false
    end
   else
    print('Unable to connect to server')
    return false
  end
end








function platoboostcheck_Keychecker(key)--Redeem your keys (generated in dashboard), and if the keys are successfully redeemed, this id will be whitelisted
  if key then
    if key ~="" then
      ditto=request({Url=string.format('https://api-gateway.platoboost.com/v1/authenticators/redeem/%d/%s/%s',platoboostid,platoboostcheckid,key),
        Method="POST"
      })
      if ditto then
        if tostring(game:GetService("HttpService"):JSONDecode(ditto.Body).success) == "true" then
          return true
         else
          return false
        end
       else
        print('Unable to connect to server')
        return false
      end

     else
      print("key is a string without anything, which is weird :< ")
      return false
    end
   else
    print("The key cannot be nil")
    return false
  end

end









--how to use
if platoboostcheck_checkwhitelisted() then
  print("whitelisted")
 else
  print("Not Whitelisted or unable to connect to server")
  setclipboard(platoboostcheck_getkeylink())--Copy key link
end
--You don't need to make a text box, that's superfluous








if platoboostcheck_Keychecker("your key") then--Replace your key with a key(such as what the user types in a text box)
  print("Successfully redeemed the key. Now the id is whitelisted")
 else
  print("Failed to redeem key, please enter the correct key")
end






--About platoboostcheck_checkwhitelisted
---If you only want users to complete your tasks (such as viewing ads), you just use platoboostcheck_checkwhitelisted
--And you don't need to add a text box for the user to enter the key


--About platoboostcheck_Keychecker
--For example, if some users don't want to complete your task, you can sell them a key(the key generated in the dashboard), they just need to enter the key, and their id will be whitelisted
--When the user has redeemed the key, the key will disappear, and the user will get the whitelist duration corresponding to the key you set
--You need to add a text box for the user to enter the key

--That's all ：>


