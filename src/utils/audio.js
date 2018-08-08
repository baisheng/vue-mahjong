/**
 处理声音的工厂方法
 **/


const  Howler = require('howler')


export const playVoice = (key) => {
  let audio_src = require(`../assets/audio/${key}.wav`)
  new Howl({
    src: audio_src
  }).play();

}
