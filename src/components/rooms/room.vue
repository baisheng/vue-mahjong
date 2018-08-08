<template>
  <div>

      <div class="header">
    <!--  <input type="text" v-model="account" class="login-input" placeholder="账号测试">
      <a href="javascript:;" @click="login">去登陆 </a>
      <br>
      <br>-->
      <code>
      &nbsp;&nbsp;{{user_info}}

      </code>
      </div>
      <br>

    <div class="open_room">
      <a href="javascript:;" @click="openNewRoom" class="">我要开房</a>
    </div>
    <br>
    <div class="space-100"></div>
    <div class="en_room" flex="main:center cross:center dir:top">
      <input type="text" v-model="roomId2">
      <a href="javascript:;" @click="enterRoomById">房号进入</a>
    </div>

    <!--<a href="javascript:;" @click="see">测试查看战绩</a>-->


    <div class="space-100"></div>

  </div>

</template>

<script>
  import {mapGetters} from 'vuex'
  import {config} from '@/utils/config'
  import brg from '../../utils/middle'

  export default {
    name: 'room',
    data() {
      return {
        roomId2: "",
        account: '',
      }
    },
    created() {
    },
    mounted() {

      console.log('room_id: '+this.$route.params.id)

      if(typeof(this.$route.params.id)!=='undefined'){
       /* setTimeout(()=>{
          this.socket.send(101, {
            "account": this.$route.params.id,
            "password": "",
            "iconUrl": "https://www.baidu.com/img/bd_logo1.png"
          })
        },400)*/
      }else{

      }


/*
//
      if (this.myself.room_id != 0) {
        this.roomId2 = this.myself.room_id
        setTimeout(() => {
          this.enterRoomById()
        }, 1500)
      }
*/


      brg.$on('protoMsg', (obj) => {
        let {id, msg} = obj;
        //查找房间返回
        if (id === 202) {
          if (msg.result) {
            this.$router.push({path: `/game/${this.room_id}`})
          } else {
            alert(msg.reason)
          }
        }
        if (id == 401) {
          this.$store.dispatch('fetchUserInfo', msg)
        }
      })
    },
    methods: {
      see(){

        this.socket.send(503)

      },

      /*********************************
       ****** 根据房号进入
       ********************************/
      enterRoomById() {
        if (this.roomId2) {
          this.socket.send(201, {
            "roomId": this.roomId2,
          })
        } else {
          alert('请先输入房号')
        }

      },
      /*********************************
       ****** 新开房
       ********************************/


      openNewRoom() {
        this.socket.send(201, {
          "roomId": 0
        })
      },
      /*********************************
       ****** 登录
       ********************************/
      login() {
        this.socket.send(101, {
          "account": this.account,
          "password": "",
          "iconUrl": "https://www.baidu.com/img/bd_logo1.png"
        })
      },

    },
    computed: {
      ...mapGetters({
        'user_info': 'user_info',
        'room_id': 'room_id'
      })
    }
  }
</script>

<style scoped lang="scss">
  body {
    width: 100%;
    overflow: hidden;
  }

  .header {
    padding: .2rem;
    a {
      color: #fff;
      text-decoration: none;
      padding: .15rem;
      border: 1px solid #fff;
      margin-left: .2rem;
      border-radius: 5px;
      font-size: .24rem;
    }
  }

  .bg {
    position: fixed;
    bottom: .7rem;
    left: 0;
    width: 100%;
    height: 9rem;
    background: url("../../assets/new/game.png");
    -webkit-background-size: 100%;
    background-size: 100%;
  }

  .login-btn {
    position: fixed;
    left: 50%;
    -webkit-transform: translateX(-50%);
    -moz-transform: translateX(-50%);
    -ms-transform: translateX(-50%);
    -o-transform: translateX(-50%);
    transform: translateX(-50%);
    top: 7.5rem;
    margin-top: .5rem;
    z-index: 100;
    a {
      text-decoration: none;
      color: #fff;
      display: inline-block;
      margin: .4rem;
      img {
        width: 3rem;
      }
    }
  }

  .open_room {
    a {
      text-align: center;
      display: inline-block;
      padding: .1rem .2rem;
      background: #e60012;
      border-radius: 8px;
      border: none;
      box-shadow: 0 0 5px rgba(0, 0, 0, .3);
      margin-top: .4rem;
    }
  }

  .en_room {
    margin: 1rem auto;
    width: 5.47rem;
    height: 3.09rem;
    background: #ffe8bc;
    border: 3px solid #500047;
    border-radius: 8px;
    box-shadow: 3px 3px 5px rgba(0, 0, 0, .2) inset;
    input {
      display: inline-block;
      width: 4.75rem;
      height: .74rem;
      background: #6a3906;
      color: #fff;
      border: 2px solid #cfa972;
      padding-left: 20px;
      box-shadow: 3px 3px 5px rgba(0, 0, 0, .2) inset;
      border-radius: 8px;

    }

    a {
      text-align: center;
      display: inline-block;
      padding: .1rem .2rem;
      background: #e60012;
      border-radius: 8px;
      border: none;
      box-shadow: 0 0 5px rgba(0, 0, 0, .3);
      margin-top: .4rem;
    }

  }

  a {
    border: 1px solid #fff;
    font-size: .28rem;
    padding: .1rem;
    border-radius: 3px;
  }

  input {
    padding: .05rem;
    border-radius: 3px;
    width: 2rem;
  }

  .login-input {
    border: 1px solid #000;
    background: #fff;
    width: 3rem;
    padding: .20rem;
    font-weight: bold;
    border-radius: 5px;
    color: #333;
    font-size: .3rem;
  }

  .game-list {
    margin-top: .1rem;
    background: #0c3f39;
    color: #fff;
    text-align: left;
    .item {
      height: 2.6rem;
      margin-bottom: .05rem;
      img {

        width: 100%;
        height: 100%;
      }
      span {
        text-align: center;
      }
    }
  }

  .test-content {
    h3 {
      font-weight: normal;
      padding: .2rem 0;
    }
    .line {
      margin-top: 20px;
      border-top: 1px dashed #068a60;
    }
    padding: .4rem;
    border-top: 1px solid #068a60;
    background: #0c3f39;
    color: #fff;
    text-align: left;
  }

  a {
    color: #fff;
    text-decoration: none;
  }

  input {
    border: 1px solid #fff;
  }


</style>
