/**
 * Created by LISHI on 2017/4/27.
 */
/**
 * Created by LISHI on 2017/4/27.
 */





export const LoadScript = (url, callback) => {
  let scriptObj = document.createElement("script")
  scriptObj.src = url
  scriptObj.type = "text/javascript"
  document.body.appendChild(scriptObj)
  if (typeof(callback) != "undefined") {
    if (scriptObj.readyState) {
      scriptObj.onreadystatechange = function () {
        if (scriptObj.readyState == "loaded" || scriptObj.readyState == "complete") {
          scriptObj.onreadystatechange = null
          callback()
        }
      };
    } else {
      scriptObj.onload = function () {
        callback()
      }
    }
  }
}
export const Int16ToBytes = (value) => {
  let arr = new Array(2)
  arr[0] = (value >> 8 * 1) % 256
  arr[1] = value % 256
  return arr
}

export const Int32ToBytes = (value) => {
  let arr = new Array(4)
  arr[0] = (value >> 8 * 3) % 256
  arr[1] = (value >> 8 * 2) % 256
  arr[2] = (value >> 8 * 1) % 256
  arr[3] = value % 256
  return arr
}

export const BytesToInt = (bytes, offset, count) => {
  let v = 0
  for (let i = offset; i < offset + count; i++) {
    v += bytes[i] << 8 * (count - 1 - (i - offset))
  }
  return v
}

export const ContactBytes = function () {


  let len = 0;

  for (let i = 0; i < arguments.length; i++) {
    len += arguments[i].length;
  }

  let arr = new Uint8Array(len);
  let index = 0;
  for (let i = 0; i < arguments.length; i++) {
    for (let j = 0; j < arguments[i].length; j++) {
      arr[index] = arguments[i][j];
      index++;
    }
  }

  return arr;
}

export const SubBytes = (btyes, start, len) => {
  let arr = new Uint8Array(len);
  for (let i = 0; i < len; i++) {
    arr[i] = btyes[start + i];
  }
  return arr;
}

export const ContactArray =function() {
  let len = 0
  for (let i = 0; i < arguments.length; i++) {
    len += arguments[i].length
  }
  let arr = new Array(len)
  let index = 0;
  for (let i = 0; i < arguments.length; i++) {
    for (let j = 0; j < arguments[i].length; j++) {
      arr[index] = arguments[i][j]
      index++
    }
  }
  return arr
}
