import * as Util from './util'
import * as Protobuf from 'protobufjs'
import {getProtoNameById} from './id_to_proto'
const protoObj = require("@/model/bundle.json")

export  default  class ProtobufUtil {
  /**
   *
   * @param id 协议id
   * @param msg  需要转码的信息
   */
  encode(id, msg, cb) {

    let root = Protobuf.Root.fromJSON(protoObj);
    let protoName = getProtoNameById(id)
    let handle = root.lookup(protoName)
    let data = handle.create(msg);
    let buffer = handle.encode(data).finish();
    let arrProtoId = Util.Int16ToBytes(id);
    let arr = Util.ContactBytes(arrProtoId, buffer);
    return cb && cb(arr)
  }

  decode(buffer, cb) {
    let root = Protobuf.Root.fromJSON(protoObj);
    let protoId = Util.BytesToInt(buffer, 0, 2)
    let msg = Util.SubBytes(buffer, 2, buffer.length - 2)
    let protoName = getProtoNameById(protoId)
    let handler = root.lookup(protoName)
    let res = handler.decode(msg)
    return cb && cb(res)
  }
}



