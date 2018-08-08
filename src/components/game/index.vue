<template>
  <div id="game">
    <!--顶部信息组件-->
    <info :name="room_id"></info>
    <!--菜单组件-->
    <menu1></menu1>
    <!--用户组件-->
    <user></user>
    <!--消息列表-->
    <msg></msg>
    <!--麦克风-->
    <mic></mic>
    <!--战绩-->
    <zhanji></zhanji>
    <!--结算组件-->
    <count></count>
    <!--开始-->
    <start></start>
    <!--中间倒计时部分-->
    <site :time="leftTime" :leave="leave"></site>
    <!--出牌-->
    <chupai></chupai>
    <!--碰的效果-->
    <option1></option1>
    <!--骰子-->
    <touzi></touzi>
    <!--分享提示-->
    <share></share>
    <!--记载页面-->
    <loding></loding>

    <!--用户详情-->
    <user-moal></user-moal>

    <!--表情-->
    <emoji></emoji>

    <!--效果-->
    <chipeng></chipeng>

    <!--banner-->

    <banner></banner>

    <!--时间结束提示-->
    <Over></Over>

    <div class="bg">
    </div>

    <div id="game_container" ref="game_container">

    </div>

  </div>


</template>

<script>
  /* 自动播放音乐效果，解决微信自动播放问题 */

  import {mapGetters} from 'vuex'
  import {playVoice} from '../../utils/audio'
  import Game from './game'
  import Info from './component/info'
  import Msg from './component/msg'
  import Mic from './component/mic'
  import Zhanji from './component/zhanji'
  import Count from './component/count'
  import Start from './component/start'
  import Menu1 from './component/menu'
  import Chupai from './component/chupai'
  import Loding from './component/load'
  import Share from './component/share'
  import Touzi from './component/touzi'
  import Option1 from './component/option'
  import User from './component/user'
  import Tip from './component/tip'
  import Site from './component/site'
  import Emoji from './component/emoji'
  import UserMoal from './component/userm'
  import Chipeng from './component/chipeng'
  import Banner from './component/baner'
  import brg from '../../utils/middle'
  import Over from './component/over'

  export default {
    name: 'game',
    data() {
      return {
        game: null,
        isFirst: false,
        isBanker: false,
        flag: true,
        myId: '',
        users: [],
        current: 0,
        leftTime: 0,
        timer: null,
        dian: '',
        isShowZhanji: false,
        sta: '',
        isshowtouzi: false,
        dian1: 1,
        dian2: 2,
        complete: true,
        roomId: 0,
        showOk: false,
        countmsg: 123,
        showCount: false,
        isShowTip: true,
        startBtn: false,
        leave: 0,

      }
    },
    //过滤器
    created() {
    },
    mounted() {

      //分享处理
      window.roomId = this.room_id

      //当前局的记录id
      window.recordId = this.room_record_id.recordId

      // alert(window.doShare)
      // alert(window.wx)


      if (typeof(window.doShare) != 'undefined') {
        window.doShare()
      }


      //获取自己的id
      this.myId = this.user_info.id;


      //初始化游戏
      this.initGame();


      //处理返回
      this.doListen()

      //渲染数据

      brg.$on('ak486',(obj)=>{
        this.game.initData(obj, (name) => {
          this.socket.send(303, {id: name})
        });
      })

      brg.$on('reconnect', ()=>{
        this.socket.send(320, {})
      });

    },


    methods: {





      //渲染游戏界面
      renderData(list) {
        this.doPositionData(list)
      },

      //对于出牌做出相应
      doAnswer(obj) {

        //胡的情况

        if (!obj) {
          return false
        }
        if (obj.flag == 2) {
//          console.log('胡牌')
          this.$store.dispatch('fetchCardType', {ishu: true, show: true, data: obj.canmeld})
        } else {
          //吃碰刚的情况
          if (obj.canmeld.length) {
            this.$store.dispatch('fetchCardType', {ishu: false, show: true, data: obj.canmeld})
          }
        }


      },
      //处理位置信息
      doPositionData(data) {
        let flag = false
        let pp = {}

        if (data.length == 1) {
          this.$store.dispatch('fetchCardType', {show: false})
          data[0].out.reverse()
          if (data[0].basis.length == 0) {
            return false
          }
          let newOne = data[0]
          this.$store.dispatch('fetchAllUserById', newOne)
          this.doAnswer(this.all_users['user1Data'])
        }


        if (data.length ===4) {
          data.forEach(v => {
            if (v.id === this.myId) {
              flag = true
            }
          })
          //是否观战的模式
          if (flag) {
            data.forEach(v => {
              v.out.reverse()

              if (v.id === this.myId) {
                pp['user5Data'] = 0;
                if (v.position === 1) {
                  pp['user1Data'] = v;
                  pp['user2Data'] = data.find(v => v.position == 2)
                  pp['user3Data'] = data.find(v => v.position == 3)
                  pp['user4Data'] = data.find(v => v.position == 4)
                } else if (v.position === 2) {
                  pp['user1Data'] = v;
                  pp['user2Data'] = data.find(v => v.position == 3)
                  pp['user3Data'] = data.find(v => v.position == 4)
                  pp['user4Data'] = data.find(v => v.position == 1)
                } else if (v.position === 3) {
                  pp['user1Data'] = v;
                  pp['user2Data'] = data.find(v => v.position == 4)
                  pp['user3Data'] = data.find(v => v.position == 1)
                  pp['user4Data'] = data.find(v => v.position == 2)
                } else if (v.position === 4) {
                  pp['user1Data'] = v;
                  pp['user2Data'] = data.find(v => v.position == 1)
                  pp['user3Data'] = data.find(v => v.position == 2)
                  pp['user4Data'] = data.find(v => v.position == 3)
                }
              }
            })
          } else {
            data.forEach(v => {
              v.out.reverse()
              if (v.id === this.ownerId) {
                pp['user5Data'] = 123;
                this.sta = ''
                if (v.position === 1) {
                  pp['user1Data'] = v;
                  pp['user2Data'] = data.find(v => v.position == 2)
                  pp['user3Data'] = data.find(v => v.position == 3)
                  pp['user4Data'] = data.find(v => v.position == 4)
                } else if (v.position === 2) {
                  pp['user1Data'] = v;
                  pp['user2Data'] = data.find(v => v.position == 3)
                  pp['user3Data'] = data.find(v => v.position == 4)
                  pp['user4Data'] = data.find(v => v.position == 1)
                } else if (v.position === 3) {
                  pp['user1Data'] = v;
                  pp['user2Data'] = data.find(v => v.position == 4)
                  pp['user3Data'] = data.find(v => v.position == 1)
                  pp['user4Data'] = data.find(v => v.position == 2)
                } else if (v.position === 4) {
                  pp['user1Data'] = v;
                  pp['user2Data'] = data.find(v => v.position == 1)
                  pp['user3Data'] = data.find(v => v.position == 2)
                  pp['user4Data'] = data.find(v => v.position == 3)
                }
              }
            })
          }
          this.$store.dispatch('fetchAllUser', pp);
        }


      },

      /**
       * 结算*/




      doDao(time) {
        this.timer && clearInterval(this.timer)
        this.timer = setInterval(() => {
          time--;
          this.$nextTick(() => {
            this.leftTime = time
          })
          if (time <= 1) {
            this.current = 0
            clearInterval(this.timer)
          }
        }, 1000)
      },


      /**
       * 初始化游戏
       */
      initGame() {
        this.game = Game.getInstance()
        this.$refs['game_container'].appendChild(this.game.app.view)
        this.game.loadRes(() => {
          this.$store.dispatch('showLoading', {show: false})
          this.socket.send(320, {})
        })
      },
      //庄家位置判断
      doBanerJudge(msg) {
        this.$store.dispatch('fetchBanker', msg)
      },
      /**
       * 监听websocket返回
       */
      doListen() {
        brg.$on('protoMsg', (obj) => {
          let {id, msg} = obj;
          //返回用户手头上牌的信息
          if (id == 307) {
            this.renderData(msg.list)
          }

          if (id == 306) {
            this.$store.dispatch('showCard', {show: false})
            this.$store.dispatch('showCount', {show: true, data: msg})
          }

          if (id == 206) {
            if (msg.result) {
              this.myself.room_id = ''
              this.game.getInit()
              this.$store.dispatch('initUserState', {})
              this.$store.dispatch('initGameState', {})
              this.$store.dispatch('deleteAllUser', {})
              this.$store.dispatch('initRoomState', {})
              this.$router.replace({path: '/room2'})
            }

          }


          // 返回牌的信息
          if (id == 308) {
            this.doBanerJudge(msg.banker)
            // 剩余牌数的
            if (msg.num) {
              this.$nextTick(() => {
                this.leave = msg.num
              })
            }
            this.$store.dispatch('fetchRoomStatus', msg)
            if (msg.lastMahjongid) {
              this.$store.dispatch('showCard', {
                card: msg.lastMahjongid,
                site: msg.lastPostion,
                show: true
              })
            }


            //庄家位置
            if (msg.banker) {
              this.$store.dispatch('fetchBanker', msg.banker)
            }
            if (msg.dicepostion == 0) {
            } else {
              if (msg.isFirst) {
                this.$store.dispatch('showTouzi', {point: msg.dicepostion, show: true})
              }
            }
          }

          //丢骰子的信息


          if (id === 319) {
            this.$store.dispatch('fetchRecord', msg.list)
          }
          //倒计时处理
          if (id === 317) {
            this.doDao(msg.leftTime)
            this.$store.dispatch('showMidSite', {
              show: true,
              position: msg.position
            })

            if (this.myId == msg.id) {
              this.current = 1
            } else {

              let cp = 1
              for (let index in this.all_users) {
                if (this.all_users[index].id === this.myId) {
                  cp = this.all_users[index].position
                }
              }


              if (cp == 1) {

                if (msg.position == 1) {
                  this.current = 1

                } else if (msg.position == 2) {
                  this.current = 2

                } else if (msg.position == 3) {
                  this.current = 3

                } else if (msg.position == 4) {
                  this.current = 4

                }
              } else if (cp == 2) {
                if (msg.position == 1) {
                  this.current = 4

                } else if (msg.position == 2) {
                  this.current = 1

                } else if (msg.position == 3) {
                  this.current = 2

                } else if (msg.position == 4) {
                  this.current = 3

                }
              } else if (cp == 3) {
                if (msg.position == 1) {
                  this.current = 3

                } else if (msg.position == 2) {
                  this.current = 4

                } else if (msg.position == 3) {
                  this.current = 1

                } else if (msg.position == 4) {
                  this.current = 2

                }
              } else if (cp == 4) {
                if (msg.position == 1) {
                  this.current = 2

                } else if (msg.position == 2) {
                  this.current = 3
                } else if (msg.position == 3) {
                  this.current = 4

                } else if (msg.position == 4) {
                  this.current = 1

                }
              }


            }


          }


        })
      },

    },
    computed: {
      ...mapGetters({
        'user_info': 'user_info',
        'all_users': 'all_users',
        'ownerId': 'ownerId',
        'room_id': 'room_id',
        'members': 'members',
        'bankerSite': 'bankerSite',
        'touzistatus': 'touzistatus',
        'room_status': 'room_status',
        'room_record_id': 'room_record_id',

      }),

    },
    components: {
      Info,
      Menu1,
      User,
      Tip,
      Site,
      Zhanji,
      Mic,
      Msg,
      Count,
      Start,
      Chupai,
      Option1,
      Share,
      Touzi,
      Loding,
      UserMoal,
      Emoji,
      Chipeng,
      Banner,
      Over
    }

  }
</script>

<style lang="scss" scoped="">

  #game_container {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    z-index: -1;
  }

  .count {
    border: 1px solid #000;
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: rgba(0, 0, 0, .1);
    z-index: 1000;

    .modal {
      background: rgba(0, 0, 0, .8);
      -webkit-transform: rotate(-90deg);
      -moz-transform: rotate(-90deg);
      -ms-transform: rotate(-90deg);
      -o-transform: rotate(-90deg);
      transform: rotate(-90deg);
      width: 11.6rem;
      height: 5rem;
      padding: .2rem;
      line-height: 1.5;
      margin-bottom: .5rem;
      border: 1px solid #fef000;
      border-radius: 8px;
      font-size: .26rem;
      position: relative;

      .b {
        position: absolute;
        bottom: .1rem;
        width: 100%;
        text-align: center;
      }

      a {
        display: inline-block;
        padding: .05rem .2rem;
        margin: .1rem;
        text-decoration: none;
        background: #f99039;
        color: #fff;
        border: 4px solid #f99039;
        font-size: .24rem;
        border-radius: 5px;
      }
    }
  }

  .start {
    position: fixed;
    bottom: 3rem;
    right: .4rem;
    text-decoration: none;
    color: #fff;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    font-size: .24rem;
    padding: .1rem .2rem;
    border-radius: 8px;
    background: #e60012;
    box-shadow: 0 0 5px rgba(0, 0, 0, .3);

  }

  .touziR {
    position: absolute;
    top: 50%;
    left: 50%;
    -webkit-transform: translate(-50%, -50%) rotate(-90deg);
    -moz-transform: translate(-50%, -50%) rotate(-90deg);
    -ms-transform: translate(-50%, -50%) rotate(-90deg);
    -o-transform: translate(-50%, -50%) rotate(-90deg);
    transform: translate(-50%, -50%) rotate(-90deg);

    div {
      display: inline-block;
      width: .8rem;
      img {
        width: 100%;
      }
    }

  }

  #game {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: #0a5335;
    z-index: 100;
    color: #fff;
    text-align: center;
    canvas {
      width: 100%;
      height: 100%;
    }

    .bg {
      position: fixed;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      z-index: -1;
      background: url("../../assets/nav_icon/bg.png") no-repeat center center;
      -webkit-background-size: cover;
      background-size: cover;
    }

  }

  .touzidian {
    position: fixed;
    top: 50%;
    left: 50%;
    padding: .2rem;
    border: 1px solid #fff;
    -webkit-transform: translate(-50%, -50%);
    -moz-transform: translate(-50%, -50%);
    -ms-transform: translate(-50%, -50%);
    -o-transform: translate(-50%, -50%);
    transform: translate(-50%, -50%);
  }

  #user {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 40px;
    background: #fff;
    border: 1px solid #fff;
    z-index: 1010;
  }

  .peng-modal {
    position: fixed;
    left: 5.2rem;
    top: 2rem;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    a {
      img {
        width: .7rem;
      }

      &:last-of-type {
        img {
          width: 1rem;
        }
      }
    }

  }

  .chi-modal {
    position: fixed;
    left: 5.2rem;
    top: 2rem;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    a {
      img {
        width: .7rem;
      }

      &:last-of-type {
        img {
          width: 1rem;
        }
      }
    }

  }

  .gang-modal {
    position: fixed;
    left: 5.2rem;
    top: 2rem;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    a {
      img {
        width: .7rem;
      }

      &:last-of-type {
        img {
          width: 1rem;
        }
      }
    }

  }

  .hu-modal {
    position: fixed;
    left: 5.2rem;
    top: 2rem;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    a {
      img {
        width: .7rem;
      }

      &:last-of-type {
        img {
          width: 1rem;
        }
      }
    }

  }

  .loading {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    color: #fff;
    z-index: 1000;
    background: #0f8433;
    p {
      -webkit-transform: rotate(90deg);
      -moz-transform: rotate(90deg);
      -ms-transform: rotate(90deg);
      -o-transform: rotate(90deg);
      transform: rotate(90deg);
    }
  }

  .time {
    border: 1px solid #fff;
    background: #000;
    border-radius: 50%;
    width: .8rem;
    height: .8rem;
    color: #fff;
    display: flex;
    justify-content: center;
    align-items: center;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    &.p1 {
      position: fixed;
      right: 15%;
      top: 45%;

    }
    &.p2 {
      position: fixed;
      left: 50%;
      -webkit-transform: translateX(-50%);
      -moz-transform: translateX(-50%);
      -ms-transform: translateX(-50%);
      -o-transform: translateX(-50%);
      transform: translateX(-50%);
      top: 20%;

    }
    &.p3 {
      position: fixed;
      left: 15%;
      top: 45%;

    }
    &.p4 {
      position: fixed;
      left: 50%;
      -webkit-transform: translateX(-50%);
      -moz-transform: translateX(-50%);
      -ms-transform: translateX(-50%);
      -o-transform: translateX(-50%);
      transform: translateX(-50%);
      bottom: 20%;

    }
  }
</style>
