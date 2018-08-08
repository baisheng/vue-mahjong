// The Vue build version to load with the `import` command
// (runtime-only or standalone) has been set in webpack.base.conf with an alias.
import Vue from 'vue'
import App from './App'
import router from './router'
import store from './store'
import brg from './utils/middle'

import '@/utils/init.css'
import '@/utils/common.scss'
import '../bower_components/animate.css/animate.min.css'
import Adapt from '@/utils/adapt'


let myself = {
  wecha_id: document.getElementById('wecha_id').value,
  room_id: document.getElementById('room_id').value,
  wecha_pic: document.getElementById('wecha_pic').value,
  nick_name: document.getElementById('nick_name').value,
  record_id: document.getElementById('record_id').value,
  order_id: document.getElementById('order_id').value,
  price: document.getElementById('price1').value,
  mi_id: document.getElementById('mi_id').value,
}

if(myself.mi_id){
  localStorage.setItem('mi_id',myself.mi_id)
}


Vue.prototype.myself = myself;

Adapt()
Vue.config.productionTip = true



//公共socket方法
import SocketApi from '@/utils/Protoful/socket'

const socket = new SocketApi();
socket.debug = true;
socket.timeoutInterval = 5400;
let heartTimer = null
socket.ws.onopen = () => {



  heartTimer && clearInterval(heartTimer)
  heartTimer = setInterval(() => {
    socket.send(107)
  }, 3000)


  if (store.getters.user_info.name) {
    console.log('重连')
    socket.send(101, {
      "account": this.myself.wecha_id,
      "name": this.myself.nick_name,
      "password": "",
      "iconUrl": this.myself.wecha_pic,
    })

  } else {
    store.dispatch('fetchConnectStatus', {show: false, msg: '哎呀'})
    console.log('首次建立链接')
    brg.$emit('connect', 'ok')

  }
  socket.heartDo()
}

//信息监听
socket.ws.onmessage = (e) => {
  socket.fetchData(e.data, (id, msg) => {

    if (id == 102) {

      if (msg.result) {
        store.dispatch('fetchConnectStatus', {show: false, msg: '哎呀'})
      }
    }

    let obj = {
      id,
      msg
    };
    brg.$emit('protoMsg', obj)
  })
}

//链接关闭
socket.ws.onclose = function () {
  // alert('哎呀，通讯掉线了')

  store.dispatch('fetchConnectStatus', {show: true, msg: '哎呀，通讯有点不稳定'})

  console.log('链接关闭了')
}



Vue.prototype.socket = socket;


new Vue({
  el: '#app',
  store,
  router,
  template: '<App/>',
  components: {App}
})
