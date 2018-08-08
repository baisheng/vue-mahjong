<template>
  <div>

    <div class="head" flex=" cross:center box:first">
      <div class="img">
        <img :src="user_info.iconUrl" alt="">
      </div>
      <div class="money" flex=" cross:top dir:top">
        <p> {{user_info.name}}</p>
        <p><img src="../../assets/dia.png" alt="">{{user_info.diamond}}  <a href="javascrpt:;">充值</a></p>
      </div>
      <div class="enter" flex=" cross:center dir:top">
        <a href="javascript:;" flex=" cross:center main:center" @click="enterRoom">房号进入</a>
        <a href="javascript:;" flex=" cross:center main:center" @click="gofeedback">反&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;馈</a>
      </div>
    </div>

    <!--<div class="space-100"></div>-->
    <div class="nav_1">
      <div class="aa">防止钓鱼网站</div>
      <img src="../../assets/new/nav1.png" alt="">
    </div>
    <div class="mj-item" flex="main:center cross:center" @click="showTime(1)">
      <img src="../../assets/new/nav2.png" alt="">
    </div>

    <!--    <div class="log">
          <img src="../../assets/nav_icon/logo.png" alt="">
        </div>-->
    <InRoom></InRoom>
    <time-c></time-c>
    <!--
        <input type="text" v-model="account">
        <a href="javascript:;" @click="login">登录</a><br>


        <a href="javascript:;" @click="query501">查询牌局501</a><br><br>
        <a href="javascript:;" @click="query503">查询战绩503</a><br><br>
        <a href="javascript:;" @click="query505">查询我开的局505</a> -->


    <!--加载-->


    <div class="loading" flex="main:center cross:center" v-show='isHavaId'>
      <img src="../../assets/nav_icon/logo.png" alt="" class="logo">

      <div class="text">
        <p>抵制不良游戏<span></span>拒绝盗版游戏</p>
        <p>注意自我保护<span></span>谨防受骗上当</p>
        <p>适度游戏益脑<span></span>沉迷游戏伤身</p>
        <p>合理安排时间<span></span>享受健康生活</p>
      </div>
    </div>


  </div>
</template>

<script>
  import {mapGetters} from 'vuex'
  import InRoom from './inroom'
  import TimeC from './time'
  import brg from '../../utils/middle'
  import Game from '../game/game'

  export default {
    name: 'room',
    data() {
      return {
        isHavaId: false,
        account: "",
        currentRoomId: '',
      }
    },
    mounted() {
      this.$store.dispatch('initUserState', {})
      this.$store.dispatch('initGameState', {})
      this.$store.dispatch('initRoomState', {})
      this.$store.dispatch('deleteAllUser', {})


      brg.$on('curRoomId', (d) => {
        this.currentRoomId = d
      })




      //如果进来的连接带参数
      if (this.myself.room_id != '') {
        this.isHavaId = true
        //只有登录了才能发送房间号
        brg.$on('login', (obj) => {
          if (obj === 'ok') {
            this.socket.send(201, {
              "roomId": this.myself.room_id,
              "type": "123",
              "longTime": "30"
            })
          }
        });
      }


//      测试专用登录
      /*brg.$on('connect', (obj) => {
        if (obj) {

          if (this.$route.params.id) {
            this.socket.send(101, {
              "account": this.$route.params.id,
              "name": this.$route.params.id,
              "password": "",
              "iconUrl": "https://www.baidu.com/img/bd_logo1.png"
            })
          }


        }
      })*/

      //监听数据返回
      brg.$on('protoMsg', (obj) => {
        let {id, msg} = obj;
        //查找房间返回
        if (id === 202) {
          this.myself.room_id = ''
          if (msg.result) {
            this.isHavaId = false

            this.$router.push({path: `/game/${this.room_id}`})
          } else {
            let a = Game.getInstance()
            a.getInit()
            this.isHavaId = false
            if (this.myself.record_id != '') {
              this.$router.push({path: `/rankd/${this.myself.record_id}`})
            } else {
              alert(msg.reason)
              this.$router.push({path: `/room2`})
            }
          }
        }

        if (id == 308) {
          this.$store.dispatch('fetchBanker', msg.banker)
          this.$store.dispatch('fetchRoomStatus', msg)
        }

        if (id == 209) {

          this.$store.dispatch('fetchMembers', msg.members)
          this.$store.dispatch('fetchRoomId', msg.roomId);
          this.$store.dispatch('fetchOwnerId', msg.ownerId);

          this.$store.dispatch('fetchRoomRecordId', msg)

//          if(!msg.isMarch){
          this.$store.dispatch('fetchRoomFirst', msg)
//          }


        }

      })


    },
    methods: {
      goBuy(index) {
        this.$router.push({name: 'order', params: {id: index}})
      },
      enterRoom() {
        this.$store.dispatch('showEnterRoom', {show: true})
      },
      showTime(type) {
        this.$store.dispatch('showEnterRoomTime', {show: true, type: type})
      },
      query501() {
        this.socket.send(508, {
          type: 1,
          content: '1',
          toid: '100000302'
        })
      },
      query503() {
        this.socket.send(503)
      },
      query505() {
        this.socket.send(505)
      },
      gofeedback(){
        this.$router.push({path: '/feedback'})
      },
      login(){
        if (this.account == '') {
          return false
        }
        this.socket.send(101, {
          "account": this.account,
          "password": "",
          "iconUrl": "https://www.baidu.com/img/bd_logo1.png"
        })
      }


    },
    computed: {
      ...mapGetters({
        'user_info': 'user_info',
        'room_id': 'room_id',
      })
    },
    components: {
      InRoom,
      TimeC
    }
  }
</script>

<style lang="scss" scoped>
  .mj-item {
    img {
      width: 100%;
    }

  }

  .nav_1 {
    position: relative;
    .aa {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: .4rem;
      line-height: .42rem;
      font-size: .2rem;
      padding-left: .2rem;
      background: rgba(0, 0, 0, .7);
    }
    img {
      width: 100%;
    }
  }

  input {
    border: 1px solid #fff;
  }

  .loading {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: #012118;
    z-index: 99999999992;

    .logo {
      width: 2.5rem;
      margin-bottom: .5rem;
    }
    .text {
      color: #219555;
      position: fixed;
      bottom: 30px;
      font-size: .28rem;
      line-height: 2;
      left: 50%;
      -webkit-transform: translateX(-50%);
      -moz-transform: translateX(-50%);
      -ms-transform: translateX(-50%);
      -o-transform: translateX(-50%);
      transform: translateX(-50%);
      width: 90%;
      text-align: center;
      span {
        display: inline-block;
        width: .4rem;
      }
    }

  }

  .head {
    height: 1.28rem;
    width: 100%;
    background: url("../../assets/nav_icon/head.png") center center;
    -webkit-background-size: 100%;
    background-size: 100%;
    font-size: .24rem;
    .img {
      width: 1rem;
      margin-left: .4rem;
      img {
        height: .8rem;
        width: .8rem;
        border: 1px solid #ffcf13;
        border-radius: .4rem;
      }
    }
    .money {
      color: #333;
      font-weight: bold;
      img {
        width: .3rem;
        height: .2rem;
      }
      p {

        margin: .036rem;
        img {
          margin-right: .2rem;
        }

        a {
          color: #fff;
          text-decoration: none;
          background: #e60012;
          display: inline-block;
          margin-left: .5rem;
          border-radius: 7px;
          line-height: 1.4;
          padding: 0 .1rem;
          font-weight: 200;
        }
        &:last-of-type {
          color: #fff;
        }

      }
    }
    .enter {
      margin-right: -.6rem;
      a {
        display: inline-block;
        background: #4c0a0c;
        width: 1.2rem;
        height: .4rem;
        margin: .1rem;
        color: #fff;
        text-decoration: none;
        font-size: .24rem;
        border-radius: 4px;
        line-height: .42rem;
        text-align: center;
        box-shadow: 2px 2px 3px rgba(0, 0, 0, 1);
      }
    }
  }

  .log {
    margin: 0 auto;
    margin-top: 3rem;
    width: 3.86rem;
    img {
      width: 100%;
      height: 100%;
    }

  }
</style>
