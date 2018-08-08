/**
 * Created by LISHI on 2017/4/27.
 */

const ss = require('./protoname')



//根据id获取Name
export const getProtoNameById = (id)=>{
  let num = ss.find(v=>v.id===id)
  return num.name
}
