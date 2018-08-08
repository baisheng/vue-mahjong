<template>
  <div id="app">
    <router-view></router-view>
    <bottom-nav></bottom-nav>
    <connect></connect>

  </div>
</template>

<script>
  import BottomNav from '@/components/common/nav'
  import Connect from '@/components/common/Connect'
  import brg from './utils/middle'

  import {playVoice} from './utils/audio'
  import $ from 'jquery'
  export default {
    name: 'app',
    components: {
      BottomNav,
      Connect
    },
    mounted() {


      //订单充值
      brg.$on('login', (obj) => {
        if (obj === 'ok') {
          brg.$emit('reconnect', obj);
          if (this.myself.order_id !== '') {
            this.socket.send(402, {
              orderid: this.myself.order_id,
              price: this.myself.price
            });

            let params = {
              memberId: this.myself.wecha_id,
              nickname: this.myself.nick_name,
              rechargeAmount: this.myself.price,
              rechargeDate: new Date(),
              orderId:this.myself.order_id,
              promoteId:localStorage.getItem('mi_id')?localStorage.getItem('mi_id'):0
            };
            $.ajax({
              url: 'http://120.76.45.115:9019/MiService/api/saveRecharge',
              type: "POST",
              data: params,
              dataType: 'JSON',
              success: function (data) {
                if (data.code == 200) {
                  alert('充值成功')
                } else {
                  alert(data.reason)
                }
              }

            })


          }
        }
      });


      //微信登录专用
      brg.$on('connect', (obj) => {
        if (obj == 'ok') {
          this.socket.send(101, {
            "account": this.myself.wecha_id,
            "name": this.myself.nick_name,
            "password": "",
            "iconUrl": this.myself.wecha_pic,
          })
        }
      })



      brg.$on('protoMsg', (obj) => {
        let {id, msg} = obj

        //更新房间状态 房间id 房主id
        if (id == 209) {
          this.$store.dispatch('fetchMembers', msg.members)
          this.$store.dispatch('fetchRoomId', msg.roomId);
          this.$store.dispatch('fetchOwnerId', msg.ownerId);

          /**
           *  required uint32 isStart = 3;            // 是否已开局 1是 0否 其实就是是不是正在牌局
              required bool isMarch = 4;              // 是不是点过开始 只有第一次现实开始按钮
           */

          if (msg.isStart == 0&&!msg.isMarch) {
            this.$store.dispatch('showStart', {show: true})
          }else{
            this.$store.dispatch('showStart', {show: false})

          }


        }
        if (id === 214) {
          if (msg.result) {
            this.$store.dispatch('showStart', {show: false})
            console.log('开局成功')
            playVoice('start')
          }
        }
        //失败原因提示

        /*   if (id == 321 || id == 312 || id==313) {
         if (!msg.result) {
         alert(msg.reason)

         }
         }*/


        if (id == 506) {
          this.$store.dispatch('fetchMyRecord', msg)
        }
        if (id == 403) {
          if (msg.result) {
            this.myself.order_id = ''
            alert('充值成功')
          } else {
            alert(msg.reason)
          }
        }


        //登录信息返回
        if (id === 102) {
          if (!msg.result) {
            alert(JSON.stringify(msg.reason))
          } else {
            brg.$emit('login', 'ok');

          }
        }

        //登录事件发射
        if (id == 401) {
          this.$store.dispatch('fetchUserInfo', msg)
        }
        if (id === 507) {
          //登录信息返回
          this.$store.dispatch('fetchRoomStatus', {
            flag: msg.flag,
            roomId: msg.roomId
          })

          if (msg.flag == 2&&this.myself.room_id=='') {
            //如果是进入房间，则重新发送一次
            this.socket.send(201, {
              roomId: msg.roomId
            })
            console.log('进入房间')

          } else {
            console.log('进入大厅')

          }


        }
      })

    },
    methods: {}
  }
</script>

<style>


  html, body {
    font-size: 16px;
    overflow: auto;
    background: url("./assets/mainbg.jpg");
    -webkit-background-size: 100%;
    background-size: 100%;

  }

  #app {
    font-family: Helvetica, Arial, sans-serif;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    position: relative;
    width: 100%;
    height: 100%;
    color: #fff;

  }
</style>
