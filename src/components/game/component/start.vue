<template>

  <div class="start_tip" v-show="showstart.show">
    <div class="status">等待房主开局</div>
    <p>本局时长{{endTime}}分钟</p>
    <p>房主：{{fangzhu}}</p>
    <p><span class="time">{{time}}</span>后未开局的房间将解散</p>
    <a href="javascript:;" class="start" @click="start" v-show="isShowBtn">开始</a>
    <a href="javascript:;" class="start" @click="invite" v-show="isShareBtn">邀请</a>


  </div>

</template>

<script>
  import brg from '../../../utils/middle'
  import {mapGetters} from 'vuex'

  import {playVoice} from '../../../utils/audio'

  var moment = require('moment');

  let timer = null

  export default {
    data() {
      return {
        fengCircle: 0,
        time1: 0,
        timer2: null,

      }
    },

    mounted() {



        brg.$on('protoMsg', (obj) => {
          let {id, msg} = obj

          if (id == 209) {
            if (msg.endTime) {
              this.timer2 && clearInterval(this.timer2)
              this.timer2 = setInterval(() => {
                this.$nextTick(() => {
                  this.time1 = (msg.endTime - moment().unix()) * 1000
                })
              }, 1000)
            }

          }

        })


    },
    methods: {
      start() {
        this.socket.send(213, {roomId: this.room_id})
      },
      invite() {
        this.$store.dispatch('showShareModal', {show: true})

      }
    },
    computed: {
      ...mapGetters({
        'showstart': 'showstart',
        'room_id': 'room_id',
        'members': 'members',
        'user_info': 'user_info',
        'room_first': 'room_first',
      }),
      isShareBtn() {
        let temp = 0
        this.members.find(v => {
          if (v.position > 0) {
            temp++;
          }
        })
        if (temp >= 4) {
          return false
        } else {
          return true
        }
      },

      fangzhu() {
        if (this.members.length) {
          return this.members[0].playerName
        }
      },
      time() {
        return moment(this.time1).format('mm:ss');
      },
      endTime(){
        return parseInt(this.room_first.longTime) / 60
      },

      isShowBtn() {

        let flag = false
        let temp = 0
        this.members.find(v => {
          if (v.PlayerId === this.user_info.id) {
            if (v.position == 1) {
              flag = true
            }
          }
          if (v.position > 0) {
            temp++;
          }
        })

        if (temp == 4 && flag) {
          return true
        } else {
          return false
        }
      }
    }
  }

</script>

<style scoped lang="scss">
  .start_tip {
    position: fixed;
    top: 50%;
    left: 70%;
    width: 100%;
    -webkit-transform: translate(-50%, -50%) rotate(-90deg);
    -moz-transform: translate(-50%, -50%) rotate(-90deg);
    -ms-transform: translate(-50%, -50%) rotate(-90deg);
    -o-transform: translate(-50%, -50%) rotate(-90deg);
    transform: translate(-50%, -50%) rotate(-90deg);
    .status {
      color: #000;
      font-weight: 800;
      font-size: .32rem;
      text-shadow: 0 0 8px rgba(0, 0, 0, .2);
      background-image: -webkit-gradient(linear, 0 0, 0 bottom, from(rgb(248, 246, 3)), to(rgb(247, 156, 3)));
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
    }
    p {
      line-height: 1.56;
      font-size: .24rem;

    }
    .time {
      color: RGB(236, 231, 0);
      margin-right: 10px;
    }
    .start {
      color: #fff;
      background: #F79C03;
      margin-top: .4rem;
      display: inline-block;
      padding: .1rem .2rem;
      font-size: .24rem;
      letter-spacing: 5px;
      text-decoration: none;
      border-radius: .1rem;
      box-shadow: 0 0 10px rgba(0, 0, 0, .3);

    }
  }

</style>
