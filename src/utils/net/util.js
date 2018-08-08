// zouv
// 2017-02-18
// 相关工具

var Util = {
    LoadScript: function(url, callback)
    {
        var scriptObj = document.createElement("script");
        scriptObj.src = url;
        scriptObj.type = "text/javascript";
        document.body.appendChild(scriptObj);
        if (typeof(callback) != "undefined")
        {
            if (scriptObj.readyState)
            {
                scriptObj.onreadystatechange = function ()
                {
                    if (scriptObj.readyState == "loaded" || scriptObj.readyState == "complete")
                    {
                        scriptObj.onreadystatechange = null;
                        callback();
                    }
                };
            } else
            {
                scriptObj.onload = function ()
                {
                    callback();
                };
            }
        }
    }
}

// 网络相关
var UtilNet  = {
    Int16ToBytes: function(value)
    {
        var arr = new Array(2);
        arr[0] = (value >> 8 * 1) % 256;
        arr[1] = value % 256;
        return arr;
    },
    Int32ToBytes: function(value)
    {
        var arr = new Array(4);
        arr[0] = (value >> 8 * 3) % 256;
        arr[1] = (value >> 8 * 2) % 256;
        arr[2] = (value >> 8 * 1) % 256;
        arr[3] = value % 256;
        return arr;
    },
    BytesToInt: function(bytes, offset, count)
    {
        var v = 0;
        for (let i = offset; i < offset + count; i++)
        {
            v += bytes[i] << 8 * (count - 1 - (i - offset));
        }
        return v;
    },
    ContactBytes: function()
    {
      var len = 0;
      for (let i = 0; i < arguments.length; i++)
      {
        len += arguments[i].length;
      }
      var arr = new Array(len);
      var index = 0;
      for (let i = 0; i < arguments.length; i++)
      {
        for (let j = 0; j < arguments[i].length; j++)
        {
          arr[index] = arguments[i][j];
          index++;
        }
      }
      return arr;
    },
    SubBytes: function(btyes, start, len)
    {
        var arr = new Uint8Array(len);
        for (i = 0; i < len; i++)
        {
            arr[i] = btyes[start + i];
        }
        return arr;
    },
    ContactArray: function()
    {
        var len = 0;
        for (i = 0; i < arguments.length; i++)
        {
            len += arguments[i].length;
        }
        var arr = new Array(len);
        var index = 0;
        for (i = 0; i < arguments.length; i++)
        {
            for (j = 0; j < arguments[i].length; j++)
            {
                arr[index] = arguments[i][j];
                index++;
            }
        }
        return arr;
    },
}
