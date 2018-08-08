import ProtobufUtil from './proto.util'
import * as Util from './util'
import {config} from '@/utils/config'

const protobufUtil = new ProtobufUtil()
const ReconnectingWebSocket = require('../reconnecting-websocket.min')


export default class SocketApi {

  constructor() {
    this.ws = new ReconnectingWebSocket(config.url);
    this.heartTimer = null
  }

  //心跳链接
  heartDo() {
    this.heartTimer && clearInterval(this.heartTimer)
    this.heartTimer = setInterval(() => {
      this.send(107)
    }, 3000)
  }

  //发送协议
  send(id, msg = {}) {
    if (id !== 107) {
      // console.log("send msg : ", id, msg);
    }
    protobufUtil.encode(id, msg, (buffer) => {
      // console.log("send msg : ", id, buffer);
      this.ws.send(buffer);
    })
  }

  //解码协议 blob流
  fetchData(data, cb) {
    if (data instanceof Blob) {
      let blob = data
      let reader = new FileReader()
      reader.readAsArrayBuffer(blob)
      reader.addEventListener('loadend', () => {
        //接受到buffer
        let buffer = new Uint8Array(reader.result)
        //协议ID
        let protoId = Util.BytesToInt(buffer, 0, 2)
        protobufUtil.decode(buffer, (msg) => {
          if (protoId != 108) {
            // console.log(protoId, msg)
          }
          return cb && cb(protoId, msg)
        })
      })
    } else {
      let err = `ERROR receive data: ${data}`;
    }
  }
}






